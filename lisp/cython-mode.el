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
   :feature 'string-interpolation
   :override t
   '((interpolation) @python--treesit-fontify-string-interpolation)

   :language 'cython
   :feature 'keyword
   '(["and" "as" "assert" "async" "await" "break" "class" "continue"
      "def" "del" "elif" "else" "except" "exec" "finally" "for"
      "from" "global" "if" "import" "in" "is" "lambda" "nonlocal"
      "not" "or" "pass" "print" "raise" "return" "try" "while"
      "with" "yield"
      ;; Cython-specific keywords
      "cdef" "cpdef" "cimport" "ctypedef" "extern" "include" "nogil"
      "gil" "inline" "public" "readonly" "api"] @font-lock-keyword-face)

   :language 'cython
   :feature 'builtin
   '((call
      function: (identifier) @font-lock-builtin-face
      (:match
       "^\\(abs\\|all\\|any\\|ascii\\|bin\\|bool\\|breakpoint\\|bytearray\\|bytes\\|callable\\|chr\\|classmethod\\|compile\\|complex\\|delattr\\|dict\\|dir\\|divmod\\|enumerate\\|eval\\|exec\\|filter\\|float\\|format\\|frozenset\\|getattr\\|globals\\|hasattr\\|hash\\|help\\|hex\\|id\\|input\\|int\\|isinstance\\|issubclass\\|iter\\|len\\|list\\|locals\\|map\\|max\\|memoryview\\|min\\|next\\|object\\|oct\\|open\\|ord\\|pow\\|print\\|property\\|range\\|repr\\|reversed\\|round\\|set\\|setattr\\|slice\\|sorted\\|staticmethod\\|str\\|sum\\|super\\|tuple\\|type\\|vars\\|zip\\|__import__\\)$"
       @font-lock-builtin-face))

     ((identifier) @font-lock-builtin-face
      (:match
       "^\\(True\\|False\\|None\\|NotImplemented\\|Ellipsis\\|__debug__\\)$"
       @font-lock-builtin-face)))

   :language 'cython
   :feature 'function
   '((function_definition
      name: (identifier) @font-lock-function-name-face)
     (c_function_definition
      name: (identifier) @font-lock-function-name-face)
     (class_definition
      name: (identifier) @font-lock-type-face))

   :language 'cython
   :feature 'assignment
   '((assignment
      left: (identifier) @font-lock-variable-name-face)
     (augmented_assignment
      left: (identifier) @font-lock-variable-name-face)
     (named_expression
      name: (identifier) @font-lock-variable-name-face))

   :language 'cython
   :feature 'definition
   '((class_definition
      body: (block
             (function_definition
              name: (identifier) @font-lock-function-name-face)))
     (parameters (identifier) @font-lock-variable-name-face)
     (parameters (typed_parameter name: (identifier) @font-lock-variable-name-face))
     (parameters (default_parameter name: (identifier) @font-lock-variable-name-face))
     (parameters (typed_default_parameter name: (identifier) @font-lock-variable-name-face))
     (keyword_separator) @font-lock-keyword-face)

   :language 'cython
   :feature 'number
   '([(integer) (float)] @font-lock-number-face)

   :language 'cython
   :feature 'property
   '((attribute
      attribute: (identifier) @font-lock-property-face)
     (call
      function: (attribute
                 attribute: (identifier) @font-lock-function-name-face)))

   :language 'cython
   :feature 'operator
   '(["==" "!=" "<" "<=" ">" ">=" "~" "+" "-" "*" "**" "/" "//" "%" "@"
      "|" "&" "^" "<<" ">>" "+=" "-=" "*=" "/=" "%=" "//=" "**=" ">>=" "<<="
      "&=" "|=" "^=" "@="] @font-lock-operator-face)

   :language 'cython
   :feature 'bracket
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

   :language 'cython
   :feature 'delimiter
   '(["," ":" ";" "."] @font-lock-delimiter-face)

   :language 'cython
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'cython
   :feature 'decorator
   '((decorator "@" @font-lock-type-face)
     (decorator
      (call function: (identifier) @font-lock-type-face))
     (decorator
      (identifier) @font-lock-type-face)))
  "Tree-sitter font-lock settings for `cython-mode'.")

;; Helper function for string interpolation (if needed)
(defun python--treesit-fontify-string-interpolation (node override start end &optional _)
  "Fontify string interpolation in NODE between START and END."
  (let ((face (if (eq (treesit-node-type (treesit-node-parent node))
                      'f_string)
                  'font-lock-constant-face
                'font-lock-string-face)))
    (treesit-fontify-with-override start end face override)))

(defun cython-mode--treesit-indent-rules ()
  "Return tree-sitter indent rules for `cython-mode'."
  `((cython
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((parent-is "module") column-0 0)
     ((parent-is "class_definition") parent-bol cython-mode-indent-offset)
     ((parent-is "function_definition") parent-bol cython-mode-indent-offset)
     ;; Cython-specific function definitions
     ((parent-is "c_function_definition") parent-bol cython-mode-indent-offset)
     ((parent-is "cpdef_function_definition") parent-bol cython-mode-indent-offset)
     ((parent-is "cdef_function_definition") parent-bol cython-mode-indent-offset)
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

  ;; Font lock - using the same feature organization as python-ts-mode
  (setq-local treesit-font-lock-settings cython-mode--treesit-font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment definition)
                (keyword string builtin)
                (assignment bracket delimiter escape-sequence number string-interpolation)
                (decorator function property operator)))

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
