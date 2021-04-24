;;; sis-multi.el --- extended sis for multi-lanugage. -*- lexical-binding: t; -*-

;; URL: https://github.com/HyunggyuJang/sis-multi
;; Created: March 11th, 2021
;; Keywords: convenience
;; Package-Requires: ((emacs "25.1"))
;; Version: 1.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This package enables less manual switch for native or OS input source (input
;; method) for multi-linguistic environment.

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'mule)

(defvar sis-english-pattern "[a-zA-Z]"
  "Pattern to identify a character as english.")
(defvar sis-english-source 'en
  "Input source for english.")
(defvar sis-hangul-pattern "[ㄱ-ㅎㅏ-ㅣ가-힣]"
  "Pattern to identify a character as korean.")
(defvar sis-hangul-source "korean-hangul"
  "Input source for hangul lang.")
(defvar sis-japanese-pattern "\\cj"
  "Pattern to identify a character as japanese.")
(defvar sis-japanese-source "japanese-skk"
  "Input source for japanese lang.")
(defvar sis-blank-pattern "[:blank:][:punct:][:digit:]"
  "Pattern to identify a character as blank.")
(defvar sis-blank-pattern-extended (concat sis-blank-pattern "[:cntrl:]")
  "Pattern to identify a character as extended blank.")

(defvar sis-context-aggressive-line t
  "Aggressively detect context across blank lines.")

(defvar sis-context-hooks
  '(evil-insert-state-entry-hook)
  "Hooks trigger the set of input source following context.")

(defvar sis-context-triggers
  '(('+org/insert-item-below 'sis--context-line nil)
    ('+org/insert-item-above 'sis--context-line nil))
  "Commands trigger the set of input source following context.

Each trigger should be a list: (FN PRE-FN-DETECTOR POST-FN-DETECTOR).
- FN: function to trigger the context following.
- PRE-FN-DETECTOR:
  - args: none
  - return:
    - nil: left the determination to later detectors.
    - 'english: English context.
    - 'other: other language context.
- POST-FN-DETECTOR:
  - args: none
  - return:
    - nil: left the determination to later detectors.
    - 'english: English context.
    - 'other: other language context.
Input source will be switched to (or (PRE-FN-DETECTOR) (POST-FN-DETECTOR)) after
FN is invoked.")

(defvar sis--context-triggers-adviced nil "Context triggers adviced.")

;;
;; Following codes are mainly about input source manager
;;

(defsubst sis--set (source)
  "Set the input source according to source SOURCE."
  (if (eq source 'en)
      (deactivate-input-method)
    (activate-input-method source)))

(defsubst sis--string-match-p (regexp str &optional start)
  "Robust wrapper of `string-match-p'.

Works when REGEXP or STR is not a string REGEXP, STR, START all has the same
meanings as `string-match-p'."
  (and (stringp regexp)
       (stringp str)
       (string-match-p regexp str start)))

;;
;; Following codes are mainly about context-mode
;;

(defsubst sis--english-p (str)
  "Predicate on STR has English characters."
  (sis--string-match-p sis-english-pattern str))

(defsubst sis--perhaps-english-p (str)
  "Predicate on STR is has no English characters."
  (not (sis--string-match-p (mapconcat #'identity (list sis-hangul-pattern sis-japanese-pattern) "\\|") str)))

(defsubst sis--hangul-p (str)
  "Predicate on STR has /hangul/ language characters."
  (sis--string-match-p sis-hangul-pattern str))

(defsubst sis--perhaps-hangul-p (str)
  "Predicate on STR is has no Hangul characters."
  (not (sis--string-match-p (mapconcat #'identity (list sis-english-pattern sis-japanese-pattern) "\\|") str)))

(defsubst sis--japanese-p (str)
  "Predicate on STR has /japanese/ language characters."
  (sis--string-match-p sis-japanese-pattern str))

(defsubst sis--perhaps-japanese-p (str)
  "Predicate on STR is has no Japanese characters."
  (not (sis--string-match-p (mapconcat #'identity (list sis-english-pattern sis-hangul-pattern) "\\|") str)))

(cl-defstruct sis-back-detect ; result of backward detect
  to ; point after first non-blank char in the same line
  char ; first non-blank char at the same line (just before position `to')
  cross-line-to ; point after first non-blank char cross lines
  cross-line-char ; first non-blank char cross lines before the current position
  )

(defun sis--back-detect-chars ()
  "Detect char backward by two steps.

  First backward skip blank in the current line,
  then backward skip blank across lines."
  (save-excursion
    (skip-chars-backward sis-blank-pattern)
    (let ((to (point))
          (char (char-before (point))))
      (skip-chars-backward sis-blank-pattern-extended)
      (let ((cross-line-char (char-before (point))))
        (make-sis-back-detect :to to
                              :char (when char (string char))
                              :cross-line-to (point)
                              :cross-line-char (when cross-line-char
                                                 (string cross-line-char)))))))

(cl-defstruct sis-fore-detect ; result of forward detect
  to ; point before first non-blank char in the same line
  char ; first non-blank char at the same line (just after position `to')
  cross-line-to ; point before first non-blank char cross lines
  cross-line-char ; first non-blank char cross lines after the current position
  )

(defun sis--fore-detect-chars ()
  "Detect char forward.

  Forward skip blank in the current line."
  (save-excursion
    (skip-chars-forward sis-blank-pattern)
    (let ((to (point))
          (char (char-after (point))))
      (skip-chars-forward sis-blank-pattern-extended)
      (let ((cross-line-char (char-after (point))))
        (make-sis-fore-detect :to to
                              :char (when char (string char))
                              :cross-line-to (point)
                              :cross-line-char (when cross-line-char
                                                 (string cross-line-char)))))))

(defun sis--context-hangul-p (back-detect fore-detect &optional position)
  "Predicate for context of hangul language.

`back-detect' BACK-DETECT and `fore-detect' FORE-DETECT are required.
If POSITION is not provided, then default to be the current position."
  (let* ((back-to (sis-back-detect-to back-detect))
         (back-char (sis-back-detect-char back-detect))
         (fore-to (sis-fore-detect-to fore-detect))
         (fore-char (sis-fore-detect-char fore-detect)))
    (cond
     (; [hangul]^
      (and (= back-to (or position (point))) (sis--hangul-p back-char))
      t)
     (; ^[hangul]
      (and (= fore-to (or position (point))) (sis--hangul-p fore-char))
      t)
     (; [hangul lang][blank or not][^][blank or not][not english]
      (and (sis--hangul-p back-char) (sis--perhaps-hangul-p fore-char))
      t)
     (; [not english][blank or not][^][blank or not][hangul lang]
      (and (sis--perhaps-hangul-p back-char) (sis--hangul-p fore-char))
      t))))

(defun sis--context-japanese-p (back-detect fore-detect &optional position)
  "Predicate for context of japanese language.

`back-detect' BACK-DETECT and `fore-detect' FORE-DETECT are required.
If POSITION is not provided, then default to be the current position."
  (let* ((back-to (sis-back-detect-to back-detect))
         (back-char (sis-back-detect-char back-detect))
         (fore-to (sis-fore-detect-to fore-detect))
         (fore-char (sis-fore-detect-char fore-detect)))
    (cond
     (; [japanese]^
      (and (= back-to (or position (point))) (sis--japanese-p back-char))
      t)
     (; ^[japanese]
      (and (= fore-to (or position (point))) (sis--japanese-p fore-char))
      t)
     (; [japanese lang][blank or not][^][blank or not][not english]
      (and (sis--japanese-p back-char) (sis--perhaps-japanese-p fore-char))
      t)
     (; [not english][blank or not][^][blank or not][japanese lang]
      (and (sis--perhaps-japanese-p back-char) (sis--japanese-p fore-char))
      t))))

(defun sis--context-english-p (back-detect fore-detect &optional position)
  "Predicate for context of English.

`back-detect' BACK-DETECT and `fore-detect' FORE-DETECT are required.
If POSITION is not provided, then default to be the current position."
  (let* ((back-to (sis-back-detect-to back-detect))
         (back-char (sis-back-detect-char back-detect))
         (fore-to (sis-fore-detect-to fore-detect))
         (fore-char (sis-fore-detect-char fore-detect)))
    (cond
     (; [english]^
      (and (= back-to (or position (point))) (sis--english-p back-char))
      t)
     (; ^[english]
      (and (= fore-to (or position (point))) (sis--english-p fore-char))
      t)
     (; [english][blank or not][^][blank or not][not other]
      (and (sis--english-p back-char) (sis--perhaps-english-p fore-char))
      t)
     (; [not other][blank or not][^][blank or not][english]
      (and (sis--perhaps-english-p back-char) (sis--english-p fore-char))
      t))))

(defmacro sis--context-smart-line (&rest langs)
  "Smart-line detection with LANGS."
  (cl-flet ((detect-based-on-char (char)
                                  (cons 'cond
                                        (mapcar
                                         (lambda (lang)
                                           `((,(intern (format "sis--%s-p" lang)) ,char)
                                             ,(intern (format "sis-%s-source" lang))))
                                         langs))))
    `(lambda (back-detect fore-detect)
       (let ((cross-line-back-to (sis-back-detect-cross-line-to back-detect))
             (cross-line-back-char (sis-back-detect-cross-line-char back-detect))
             (cross-line-fore-to (sis-fore-detect-cross-line-to fore-detect))
             (cross-line-fore-char (sis-fore-detect-cross-line-char fore-detect)))
         (cond
          ((and (> cross-line-back-to (line-beginning-position 0))
                (< cross-line-back-to (line-beginning-position)))
           ,(detect-based-on-char 'cross-line-back-char))
          ((and (< cross-line-fore-to (line-end-position 2))
                (> cross-line-fore-to (line-end-position)))
           ,(detect-based-on-char 'cross-line-fore-char))
          ((and sis-context-aggressive-line
                (< cross-line-back-to (line-beginning-position)))
           ,(detect-based-on-char 'cross-line-back-char))
          ((and sis-context-aggressive-line
                (> cross-line-fore-to (line-end-position)))
           ,(detect-based-on-char 'cross-line-fore-char)))))))

(defun sis--context-line ()
  "Line context."
  (let ((line (thing-at-point 'line t)))
    (cond
     (; has /hangul/ lang char
      (sis--hangul-p line)
      sis-hangul-source)
     (; has /japanese/ lang char
      (sis--japanese-p line)
      sis-japanese-source)
     (; has no /other/ lang char
      (sis--english-p line)
      sis-english-source))))

(defvar sis-context-detectors
  (list (lambda (back-detect fore-detect)
          (when (sis--context-english-p back-detect fore-detect)
            sis-english-source))
        (lambda (back-detect fore-detect)
          (when (sis--context-hangul-p back-detect fore-detect)
            sis-hangul-source))
        (lambda (back-detect fore-detect)
          (when (sis--context-japanese-p back-detect fore-detect)
            sis-japanese-source))
        (sis--context-smart-line english hangul japanese))

  "Detectors to detect the context.

Each detector should:
- have two arguments:
  - back-detect: which is the result of (sis--back-detect-chars).
  - fore-detect: which is the result of (sis--fore-detect-chars).
- return one of the following values:
  - nil: left the determination to later detectors.
  - 'english: English context.
  - 'other: other language context.")

(defun sis--context-guess ()
  "Guest the lang context for the current point."
  (let* ((back-detect (sis--back-detect-chars))
         (fore-detect (sis--fore-detect-chars))
         (context nil))

    (when sis-context-detectors
      (dolist (detector sis-context-detectors)
        (setq context (or context (funcall detector back-detect fore-detect)))))

    context))

(defun sis--change-input-method-advice (orig-fn &rest args)
  "Suppress transient functions to be run by deactivation method.
ORIG-FN assumed to be `activate-input-method', and ARGS is its arguments."
  (let (input-method-deactivate-hook)
    (apply orig-fn args)))

;;;###autoload
(define-minor-mode sis-context-mode
  "Switch input source smartly according to context."
  :init-value nil

  (dolist (hook sis-context-hooks)
    (add-hook hook #'sis-context nil t))

  (advice-add 'activate-input-method :around #'sis--change-input-method-advice)

   ;; adviced for all, but only take effect when sis-context-mode is enabled
   (unless sis--context-triggers-adviced
     (setq sis--context-triggers-adviced t)
     (dolist (trigger sis-context-triggers)
       (let* ((trigger-fn (nth 0 trigger))
              (pre-detector (nth 1 trigger))
              (post-detector (nth 2 trigger))
              (advice-name (format "sis--context-trigger-advice-%s"
                                   (symbol-name (eval trigger-fn)))))
         ;; dynamically create the advice
         (defalias (intern advice-name)
           `(lambda (fn &rest args)
              (if sis-context-mode
                  (let ((pre-context (and (functionp ,pre-detector)
                                          (funcall ,pre-detector)))
                        (res (apply fn args))
                        (post-context (and (functionp ,post-detector)
                                           (funcall ,post-detector))))
                    (sis--set (or pre-context post-context))
                    res)
                (apply fn args))))
         ;; Add special property to the advice, so it can be easily removed
         (put (intern advice-name) 'sis--context-trigger-advice t)
         (advice-add (eval trigger-fn) :around (intern advice-name))))))

;;;###autoload
(define-globalized-minor-mode
  sis-global-context-mode
  sis-context-mode
  sis-context-mode)

;;;###autoload
(defun sis-context ()
  "Follow the context to switch input source."
  (let ((context (sis--context-guess)))
    (when context
      (sis--set context))))

(provide 'sis-multi)
;;; sis-multi.el ends here
