;;; init.el

;;; code:

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 1)
(show-paren-mode 1)

(setq column-number-mode t)

(setq-default fill-column 80)
(setq-default inhibit-startup-screen t)
(setq-default show-paren-delay 0)

(global-set-key (kbd "C-#") 'comment-or-uncomment-region)
(global-set-key (kbd "C-u") '(lambda () (interactive) (kill-line 0)))
(global-set-key (kbd "C-l") 'comint-clear-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("groups"
	 ("emacs"          (mode . emacs-lisp-mode))
	 ("python"         (mode . python-mode))
	 ("python-console" (mode . inferior-python-mode))
	 ("R"              (mode . ess-mode))
	 ("stan"           (mode . stan-mode))
	 ("R-console"      (mode . inferior-ess-mode))
         ("C++"            (or (mode . c-mode)
			       (mode . c++-mode)))
	 ("cmake"          (mode . cmake-mode))
	 ("make"           (name . "[mM]akefile"))
	 ("magit"          (name . "\*magit"))
	 ("help"           (or (name . "\*Help\*")
			       (name . "\*Apropos\*")
			       (name . "\*info\*")))
	 )))

(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-switch-to-saved-filter-groups "groups")))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(defun my-c-mode-common-hook ()
  (c-set-offset 'comment-intro 0)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0)

  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
  (setq c-file-style "stroustrup")
  (setq tab-stop-list (number-sequence 2 200 2))
  (setq tab-width 4)
  (setq indent-tabs-mode nil))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(use-package diminish
  :ensure t
  :config
  (eval-after-load "abbrev" '(diminish 'abbrev-mode)))

(use-package bind-key :ensure t)

(use-package zenburn-theme
  :ensure t
  :init
  (load-theme 'zenburn t))

(use-package clang-format :ensure t)

(use-package stan-snippets :ensure t)

(use-package stan-mode :ensure t)

(use-package helm
  :ensure t
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files))

(use-package swiper
  :ensure t
  :diminish
  :bind
  ("C-s" . swiper))

(use-package nyan-mode
  :ensure t
  :init (add-hook 'prog-mode-hook #'nyan-mode)
  :config (nyan-mode))

(use-package smartparens
  :ensure t
  :diminish
  :init (smartparens-global-mode)
  :bind
  ("M-f" . sp-forward-sexp)
  ("M-b" . sp-backward-sexp))

(use-package undo-tree
  :ensure t
  :diminish
  :init
  (global-undo-tree-mode 1))

(use-package smooth-scrolling
  :ensure t
  :init (smooth-scrolling-mode 1))

(use-package buffer-move
  :ensure t
  :init
  (setq buffer-move-behavior 'move)
  :bind
  ("C-c <up>" . buf-move-up)
  ("C-c <down>" . buf-move-down)
  ("C-c <left>" . buf-move-left)
  ("C-c <right>" . buf-move-right))

(use-package windmove
  :ensure t
  :bind
  ("C-c 5" . windmove-up)
  ("C-c 2" . windmove-down)
  ("C-c 1" . windmove-left)
  ("C-c 3" . windmove-right))

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))

(use-package iedit
  :ensure t
  :bind
  ("C-c ;" . iedit-mode))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-c e" . mc/edit-lines)
  ("C-c a" . mc/mark-all-symbols-like-this))

(use-package drag-stuff
  :ensure t
  :bind
  ("M-<up>" . drag-stuff-up)
  ("M-<down>" . drag-stuff-down))

(use-package yasnippet-snippets :ensure t)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-c TAB") 'yas-expand)
  :config (yas-reload-all))

(use-package ess
  :ensure t
  :init
  (require 'ess-site)
  (ess-toggle-underscore nil)
  (setq ess-eval-visibly-p 'nowait)
  (setq ess-S-assign-key (kbd "C-="))
  (ess-toggle-S-assign-key t)
  (ess-toggle-S-assign-key t))

(use-package julia-mode :ensure t)

(use-package treemacs
  :ensure t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-change-root-without-asking nil
          treemacs-collapse-dirs              3
          treemacs-file-event-delay           5000
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-never-persist              nil
          treemacs-no-png-images              nil
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          nil
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    (treemacs-filewatch-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ([f8]      . treemacs-toggle)
        ("C-c b"   . treemacs-bookmark)
        ("C-c C-c" . treemacs-change-root)
        ("C-c C-v" . treemacs-goto-parent-node)
        ("M-0"     . treemacs-select-window)))

(use-package cmake-mode
  :ensure t
  :init (setq cmake-tab-width 4))

(use-package cmake-font-lock
  :ensure t
  :hook cmake-mode-hook)

(use-package company
  :ensure t
  :init (global-company-mode)
  :bind ("<C-tab>" . company-complete)
  :custom
  (company-idle-delay nil)
  (company-minimum-prefix-length 2)
  (company-tooltip-limit 20)
  (company-show-numbers t)
  (company-dabbrev-downcase nil))

(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package elpy
  :ensure t
  :diminish
  :init
  (setq python-shell-interpreter "python3")
  (setq elpy-rpc-python-command "python3")
  (elpy-enable)
  (delete `elpy-module-highlight-indentation elpy-modules)
  :bind
  ("C-b" . elpy-autopep8-fix-code))

(use-package pip-requirements :ensure t)

(use-package duplicate-thing
  :ensure t
  :bind
  ("C-d" . duplicate-thing))

(use-package doxymacs
  :load-path "~/.emacs.d/builds/doxymacs/install/share/emacs/site-lisp"
  :diminish
  :init
  (add-hook 'c-mode-common-hook'doxymacs-mode)
  (defun my-doxymacs-font-lock-hook ()
    (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
	(doxymacs-font-lock)))
  (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
  (setq doxymacs-doxygen-style "C++")
  (setq doxymacs-command-character "\\"))

;; ispc
(add-to-list 'auto-mode-alist '(".ispc$" . c++-mode))
(add-to-list 'auto-mode-alist '(".isph$" . c++-mode))

(use-package polymode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode)))

(use-package flycheck
  :diminish
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-julia
  :ensure t
  :init
  (flycheck-julia-setup))

(use-package flycheck-yamllint
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode))
  :init
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))
  (add-hook 'yaml-mode-hook 'flycheck-mode))

(use-package lsp-ui
  :ensure t
  :init
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-symbol t))

(use-package lsp-mode
  :ensure t
  :diminish
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (setq lsp-highlight-symbol-at-point nil))

(use-package cquery
  :ensure t
  :init
  (defun my-cpp-setup ()
    (condition-case nil (lsp-cquery-enable) (user-error nil))
    (local-set-key (kbd "C-b") 'clang-format-buffer)
    (define-key c++-mode-map (kbd "C-d") 'duplicate-thing))
  (add-hook 'c-mode-common-hook #'my-cpp-setup))

(use-package company-lsp
  :ensure t
  :init
  (push 'company-lsp company-backends)
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))

;;; init.el ends here
