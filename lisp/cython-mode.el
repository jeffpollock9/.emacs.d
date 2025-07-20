;;; cython-mode.el --- Major mode for Cython using tree-sitter -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Generated
;; Version: 0.1.0
;; Keywords: languages, cython
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; A minimal Emacs major mode for Cython using tree-sitter.
;; Requires the tree-sitter grammar from https://github.com/b0o/tree-sitter-cython

;;; Code:

(require 'treesit)

(defcustom cython-mode-indent-offset 4
  "Number of spaces for each indentation step in `cython-mode'."
  :type 'integer
  :safe 'integerp
  :group 'cython)

(defvar cython-mode--treesit-font-lock-settings
  (treesit-font-lock-rules
   :language 'cython
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'cython
   :feature 'string
   '((string) @font-lock-string-face)

   :language 'cython
   :feature 'function
   '((function_definition name: (identifier) @font-lock-function-name-face))

   :language 'cython
   :feature 'variable
   '((identifier) @font-lock-variable-name-face)

   :language 'cython
   :feature 'number
   '([(integer) (float)] @font-lock-number-face)

   :language 'cython
   :feature 'class
   '((class_definition name: (identifier) @font-lock-type-face)))
  "Tree-sitter font-lock settings for `cython-mode'.")

(defun cython-mode--treesit-indent-rules ()
  "Return tree-sitter indent rules for `cython-mode'."
  `((cython
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((parent-is "module") column-0 0)
     ((parent-is "class_definition") parent-bol cython-mode-indent-offset)
     ((parent-is "function_definition") parent-bol cython-mode-indent-offset)
     ((parent-is "if_statement") parent-bol cython-mode-indent-offset)
     ((parent-is "elif_clause") parent-bol cython-mode-indent-offset)
     ((parent-is "else_clause") parent-bol cython-mode-indent-offset)
     ((parent-is "for_statement") parent-bol cython-mode-indent-offset)
     ((parent-is "while_statement") parent-bol cython-mode-indent-offset)
     ((parent-is "try_statement") parent-bol cython-mode-indent-offset)
     ((parent-is "except_clause") parent-bol cython-mode-indent-offset)
     ((parent-is "finally_clause") parent-bol cython-mode-indent-offset)
     ((parent-is "with_statement") parent-bol cython-mode-indent-offset)
     ((parent-is "block") parent-bol cython-mode-indent-offset)
     ((parent-is "list") parent-bol cython-mode-indent-offset)
     ((parent-is "tuple") parent-bol cython-mode-indent-offset)
     ((parent-is "dictionary") parent-bol cython-mode-indent-offset)
     ((parent-is "set") parent-bol cython-mode-indent-offset)
     ((parent-is "parenthesized_expression") parent-bol cython-mode-indent-offset)
     ((parent-is "argument_list") parent-bol cython-mode-indent-offset)
     ((parent-is "parameters") parent-bol cython-mode-indent-offset)
     (catch-all parent-bol 0))))

;;;###autoload
(define-derived-mode cython-mode prog-mode "Cython"
  "Major mode for editing Cython files using tree-sitter."
  :group 'cython

  ;; Require tree-sitter support
  (unless (treesit-ready-p 'cython)
    (error "Tree-sitter for Cython isn't available"))

  ;; Set up tree-sitter
  (treesit-parser-create 'cython)

  ;; Font lock
  (setq-local treesit-font-lock-settings cython-mode--treesit-font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment string)
                (function class)
                (variable number)))

  ;; Indentation
  (setq-local treesit-simple-indent-rules (cython-mode--treesit-indent-rules))

  ;; Imenu (for function/class navigation)
  (setq-local treesit-simple-imenu-settings
              '(("Function" "\\`function_definition\\'" nil nil)
                ("Class" "\\`class_definition\\'" nil nil)))

  ;; Comments
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local comment-end "")

  ;; Electric pairs
  (setq-local electric-indent-chars (append ":" electric-indent-chars))

  ;; Enable tree-sitter font lock
  (treesit-major-mode-setup)
  
  ;; Force font-lock to be enabled
  (font-lock-mode 1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))

(provide 'cython-mode)

;;; cython-mode.el ends here
