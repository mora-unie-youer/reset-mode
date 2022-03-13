;;; reset-mode.el --- Major mode for editing Reset language source code -*- lexical-binding: t -*-
;;
;; Filename: reset-mode.el
;; Description: Major mode for editing Reset language source code
;; Author: Mora Unie Youer <mora_unie_youer@riseup.net>
;; Maintainer: Mora Unie Youer <mora_unie_youer@riseup.net>
;; Copyright (c) 2022 Mora Unie Youer
;; Created: Mar 12 2022
;; URL: https://github.com/mora-unie-youer/reset-mode
;;      https://gitlab.com/mora-unie-youer/reset-mode
;;      https://notabug.org/mora-unie-youer/reset-mode
;;      https://codeberg.org/mora-unie-youer/reset-mode

;;; Commentary:
;;
;; Major mode for editing Reset language source code.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(defgroup reset nil
  "Major mode for editing Reset source code."
  :prefix "reset-"
  :group 'languages)

(defvar reset-mode-syntax-table
  (let ((table (make-syntax-table)))
    ; Adding comments to our syntax table
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    ; Highlight chars as strings
    (modify-syntax-entry ?' "\"" table)
    table)
  "Syntax table to use in Reset mode.")

(defcustom reset-indent-tabs-mode nil
  "Indentation can insert tabs in Reset mode if this is non-nil."
  :type 'boolean
  :safe 'booleanp)

(defconst reset-font-lock-keywords
  `(;; Keywords
    ;; Labels
    ("^\\s *label\\s +\\([a-zA-Z0-9_-]+\\)"
     1 font-lock-function-name-face)
    (,(regexp-opt
       '("if" "else"                    ; Conditionals
         "break" "while"                ; Loops
         "allocate" "return" "syscall"  ; Functions-related
         "global" "goto" "label"        ; Labels
         "readchar" "writechar"         ; Byte operations
         "char" "int"                   ; Array types
         "include")                     ; File operations
       'symbols)
     (1 font-lock-keyword-face))
    ;; Variables
    ("\\_<\\([ax][0-9]+\\)"
     0 font-lock-variable-name-face)
    ;; Constants
    ("\\_<\\([A-Z]+\\(\\w\\|_\\)*\\)"
     1 font-lock-type-face))
  "Additional expressions to highlight in Reset mode.")

(defun reset-mode-variables ()
  "Set up initial buffer-local variables for Ruby mode."
  (setq indent-tabs-mode reset-indent-tabs-mode)
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+ *")
  (setq-local parse-sexp-ignore-comments t)
  (setq-local parse-sexp-lookup-properties t))

;;;###autoload
(define-derived-mode reset-mode prog-mode "Reset"
  "Major mode for editing Reset source code."
  (reset-mode-variables)
  (setq-local font-lock-defaults '((reset-font-lock-keywords) nil nil
                                   ((?_ . "w")))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rt\\'" . reset-mode))

(provide 'reset-mode)

;;; reset-mode.el ends here
