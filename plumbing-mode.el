;;; plumbing-mode.el --- font lock for Plan 9's plumber configuration files

;; Copyright © 2017, by diamond-lizard

;; Author: diamond-lizard
;; Version: 0.0.1
;; Keywords: color, convenience

;; This file is not part of GNU Emacs.

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Affero General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Affero General Public License for more details.
;;
;;  You should have received a copy of the GNU Affero General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Syntax highlighting for Plan 9's plumber configuration files
;; https://9fans.github.io/plan9port/man/man7/plumb.html


;;; Code:

(eval-when-compile
  (require 'rx))


;;; Customization
(defgroup plumbing nil
  "Plumbing mastering in Emacs"
  :prefix "plumbing-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/diamond-lizard/plumbing-mode")
  :link '(emacs-commentary-link :tag "Commentary" "plumbing-mode"))


;;; Font locking
(setq plumbing-mode-syntax-table
  (let ((table (make-syntax-table)))
    table))
;  "Syntax table in use in `plumbing-mode' buffers.")

(defface plumbing-string-delimiter
  '((t :inherit font-lock-negation-char-face))
  "Face for string delimiters in plumbing-mode."
  :group 'plumbing)

(defface plumbing-string
  '((t :inherit font-lock-string-face))
  "Face for strings in plumbing-mode."
  :group 'plumbing)

(defface plumbing-equals-sign
  '((t :inherit font-lock-builtin-face))
  "Face for equals-sign in plumbing-mode."
  :group 'plumbing)

(defface plumbing-message-type
  '((t :inherit font-lock-keyword-face))
  "Face for message-type keywords in plumbing-mode."
  :group 'plumbing)

(defface plumbing-variable-name
  '((t :inherit font-lock-variable-name-face))
  "Face for variable names in plumbing-mode."
  :group 'plumbing)

(defface plumbing-verb
  '((t :inherit font-lock-preprocessor-face))
  "Face for verb keywords in plumbing-mode."
  :group 'plumbing)

(defface plumbing-object
  '((t :inherit font-lock-constant-face))
  "Face for object keywords in plumbing-mode."
  :group 'plumbing)

(defface plumbing-action-verb
  '((t :inherit font-lock-type-face))
  "Face for action verb keywords in plumbing-mode."
  :group 'plumbing)

(defface plumbing-text
  '((t :inherit font-lock-function-name-face))
  "Face for text literal in plumbing-mode."
  :group 'plumbing)

(defface plumbing-variable-assignment
  '((t :inherit font-lock-variable-name-face))
  "Face for variable assignment in plumbing-mode."
  :group 'plumbing)


;;; Specialized rx

(eval-when-compile
  (defun plumbing-rx-symbol (form)
    "Translate FORM into a regular expression."
    (let ((body (cdr form)))
      (rx-to-string `(and symbol-start ,@body symbol-end) 'no-group)))

  (setq plumbing-rx-constituents
    `((symbol plumbing-rx-symbol 0 nil)
      (string-delimiter  . ,(rx "'"))
      (string       . ,(rx "'"
                           (group (one-or-more (not (any "'"))))
                           "'"))
      (equals-sign   . ,(rx "="))
      (message-type . ,(rx (or "arg"
                               "attr"
                               "data"
                               "dst"
                               "ndata"
                               "src"
                               "type"
                               "wdir")))
      (verb         . ,(rx (or "add"
                               "delete"
                               "is"
                               "isdir"
                               "isfile"
                               "matches"
                               "set")))
      (object       . ,(rx (or "plumb")))
      (action-verb  . ,(rx (or "client"
                               "start"
                               "to")))
      (text         . ,(rx bol "type is " (group "text") eol))
    "Additional special sexps for `plumbing-rx'"))

  (defmacro plumbing-rx (&rest sexps)
    "Specialized `rx' variant for plumbing-mode.

In addition to the standard forms of `rx', the following forms
are available:

`(symbol SEXP …)'
     Match SEXPs inside symbol boundaries only

`message-type'
     Any valid plumbing message type

`verb'
     Any built-in plumbing verb

`object'
     Any built-in plumbing object

`action-verb'
     Any built-in plumbing action verb

`variable-name'
     Any variable name, without a leading dollar sign"
    (let ((rx-constituents (append plumbing-rx-constituents rx-constituents)))
      (cond ((null sexps)
             (error "No regexp"))
            ((cdr sexps)
             (rx-to-string `(and ,@sexps) t))
            (t
             (rx-to-string (car sexps) t))))))


(setq plumbing-font-lock-keywords
  `(
    ;; comments
    (,"^#.*" 0 font-lock-comment-face)
    ;; strings
    (,(plumbing-rx string) 1 'plumbing-string)
    ;; string delimiters
    (,(plumbing-rx string-delimiter) 0 'plumbing-string-delimiter)
    ;; equals sign
    (,(plumbing-rx equals-sign) 0 'plumbing-equals-sign)
    ;; message types
    (,(plumbing-rx (symbol message-type)) 0 'plumbing-message-type)
    ;; Variables
    (,"\$[A-Za-z0-9_]+" 0 'plumbing-variable-name)
    ;; Verbs
    (,(plumbing-rx (symbol verb)) 0 'plumbing-verb)
    ;; Objects
    (,(plumbing-rx (symbol object)) 0 'plumbing-object)
    ;; Action verbs
    (,(plumbing-rx (symbol action-verb)) 0 'plumbing-action-verb)
    ;; Text
    (,(plumbing-rx text) 1 'plumbing-text)
    ;; Variable assignment
    ("\\([^[:blank:]=\n]+\\)[[:blank:]]*?=" 1 'plumbing-variable-assignment)))
;  "Font lock keywords for plumbing-mode.")

;;;###autoload
(define-derived-mode plumbing-mode prog-mode "Plumbing" ()
  "Major mode for syntax highlighting of Plan 9's plumber configuration files.

\\{plumbing-mode-map}"
;; Font locking
(setq font-lock-defaults '((plumbing-font-lock-keywords) nil nil))
;;                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^  ^^^ ^^^
;;                                      /                 \   \
;;                       name of variable which holds      |   \__ case-insensitive
;;                         our mode's keywords             |        keywords?
;;                                                      disable
;;                                                    font lock?
)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.plumbing\\'" . plumbing-mode))

(provide 'plumbing-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; plumbing-mode.el ends here
