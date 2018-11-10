;;; lumen-mode.el --- a major-mode for editing Lumen code

;; Copyright © 2018 Zhao Shenyang

;; Author: Zhao Shenyang
;; URL: https://gitlab.com/larme/lumen-mode
;; Version: 0.0.1
;; Created: 2018-10-18
;;
;; Keywords: languages, tools

;;; Commentary:

;; Provides font-lock and indentation for editing lumen code.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:


(defvar lumen-keywords
  '(;; define-special words in compiler.l
    "do" "while" "break" "return" "new" "typeof" "throw" "get"

    ;; macros.l
    "quote" "quasiquote" "set" "at" "wipe" "list"
    "if" "case" "when" "unless"
    "obj"
    "let" "with" "let-when"
    "define-macro" "define-special" "define-symbol" "define-reader"
    "define" "define-global"
    "with-frame" "with-bindings"
    "let-macro" "let-symbol" "let-unique"
    "fn" "apply"
    "guard" "each" "for" "step"
    "set-of"
    "language" "target"
    "join!" "cat!" "inc" "dec"
    "with-indent"
    "export" "when-compiling" "during-compilation"

    "or" "and" "true" "false" "nil"
    "." "+" ".." "^" "-" "*" "%" "/" ">" "<" ">=" "<=" "=" "~=" "#" "..." ":"))

(defvar lumen-builtins
  '(;; lua builtin
    "_G" "_VERSION" "arg" "assert" "bit32" "collectgarbage" "coroutine" "debug"
    "dofile" "getfenv" "getmetatable" "io" "ipairs" "load" "loadfile"
    "loadstring" "math" "next" "os" "package" "pairs" "pcall" "rawequal"
    "rawget" "rawlen" "rawset" "require" "select" "setfenv" "setmetatable"
    "string" "table" "unpack" "xpcall"

    ;; functions in runtime.l
    "environment" "target"
    "nil?" "is?" "no" "yes" "either" "has?"
    "#" "none?" "some?" "one?" "two?" "hd"
    "type" "string?" "number?" "boolean?" "function?" "obj?" "atom?"
    "hd?" "nan" "inf" "-inf" "nan?" "inf?"
    "clip" "cut"
    "keys" "edge" "inner" "tl" "char" "code"
    "string-literal?" "id-literal?"
    "add" "drop" "last" "almost" "reverse"
    "reduce" "join" "find" "first" "in?" "pair" "sort" "map" "keep"
    "key?" "empty?" "stash" "unstash" "destash!"
    "search" "split"
    "pairwise" "number" "number-code?" "numeric?"
    "tostring" "escape" "str"
    "apply" "call"
    "setenv" "print" "error"
    "abs" "acos" "asin" "atan" "atan2" "ceil" "cos" "floor" "log" "log10"
    "max" "min" "pow" "random" "sin" "sinh" "sqrt" "tan" "tanh" "trunc"))

(defvar lumen-local-fn-pattern
  (rx (syntax open-parenthesis)
      (or "define-global" "define" "set") (1+ space)
      (group (1+ (or (syntax word) (syntax symbol) "-" "_")))
      (0+ (syntax whitespace)) ;; newline will cause this to not match
      (syntax open-parenthesis) (or "fn" "lambda" "λ")))

(defvar lumen-defn-pattern
  (rx (syntax open-parenthesis) "defn" (1+ space)
      (group (1+ (or (syntax word) (syntax symbol) "-" "_")))))

(defvar lumen-font-lock-keywords
  (eval-when-compile
    `((,lumen-local-fn-pattern 1 font-lock-variable-name-face)
      (,lumen-defn-pattern 1 font-lock-variable-name-face)
      (,(rx (syntax open-parenthesis)
            (or "fn" "lambda" "λ") (1+ space)
            (group (and (not (any "("))
                        (1+ (or (syntax word) (syntax symbol))))))
       1 font-lock-variable-name-face)
      (,(regexp-opt lumen-keywords 'symbols) . font-lock-keyword-face)
      (,(regexp-opt lumen-builtins 'symbols) . font-lock-builtin-face)
      (,(rx (group ":" (1+ word))) 0 font-lock-builtin-face)
      (,(rx (group letter (0+ word) "." (1+ word))) 0 font-lock-type-face))))

(defun lumen-font-lock-setup ()
  (setq font-lock-defaults
        '(lumen-font-lock-keywords nil nil (("+-*/.<>=!?$%_&:" . "w")))))

;; simplified version of lisp-indent-function
(defun lumen-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (let* ((fn (buffer-substring (point) (progn (forward-sexp 1) (point))))
           (open-paren (elt state 1))
           (method (get (intern-soft fn) 'lumen-indent-function)))
      (cond ((member (char-after open-paren) '(?\[ ?\{))
             (goto-char open-paren)
             (1+ (current-column)))
            ((eq method 'defun)
             (lisp-indent-defform state indent-point))
            ((integerp method)
             (lisp-indent-specform method state indent-point normal-indent))
            (method
             (funcall method indent-point state))))))

(defun lumen-paredit-setup () 1)
;;;###autoload
(define-derived-mode lumen-mode lisp-mode "Lumen"
  "Major mode for editing Lumen code.

\\{lumen-mode-map}"
  ;; TODO: completion using inferior-lisp
  (make-local-variable 'lumen-module-name)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'lisp-indent-function) 'lumen-indent-function)
  ;; (set (make-local-variable 'inferior-lisp-program) "lumen")
  (set-syntax-table lumen-mode-syntax-table)
  (lumen-font-lock-setup)
  (add-hook 'paredit-mode-hook #'lumen-paredit-setup))

(put 'fn 'lumen-indent-function 'defun)
(put 'define 'lumen-indent-function 'defun)
(put 'define-macro 'lumen-indent-function 'defun)
(put 'define-global 'lumen-indent-function 'defun)
(put 'each 'lumen-indent-function 'defun)
(put 'do 'lumen-indent-function 0)
(put 'let 'lumen-indent-function 1)
(put 'when 'lumen-indent-function 1)
(put 'for 'lumen-indent-function 1)


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lumen\\'" . lumen-mode))

(provide 'lumen-mode) ;;; lumen-mode.el ends here
