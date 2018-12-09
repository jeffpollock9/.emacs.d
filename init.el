;;; init.el

;;; code:

;; TOOD: take ideas from https://github.com/malb/emacs.d/blob/master/malb.org

(let ((file-name-handler-alist nil)
      (gc-cons-threshold (* 1024 1024 100)))

  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file)

  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa"     . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("gnu"       . "http://elpa.gnu.org/packages/"))
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
  (setq-default show-paren-delay 0)
  (setq-default major-mode 'text-mode)

  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "C-#") 'comment-or-uncomment-region)
  (global-set-key (kbd "C-u") '(lambda () (interactive) (kill-line 0)))
  (global-set-key (kbd "C-l") 'comint-clear-buffer)
  (global-set-key (kbd "M-l") 'toggle-truncate-lines)

  (define-minor-mode resize-mode
    "Toggle resizing of current window"
    :init-value nil
    :lighter " resize"
    :keymap
    '(([left] . shrink-window-horizontally)
      ([right] . enlarge-window-horizontally)
      ([up] . enlarge-window)
      ([down] . shrink-window)
      ([return] . resize-mode)))

  (global-set-key (kbd "C-c r") 'resize-mode)

  (setq ibuffer-expert t
        ibuffer-show-empty-filter-groups nil
        ibuffer-saved-filter-groups
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

  (setq ispell-program-name "hunspell"
        ispell-local-dictionary "en_GB")

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

  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)

  (global-auto-revert-mode 1)

  (use-package diminish
    :ensure t
    :diminish abbrev-mode)

  (use-package bind-key
    :ensure t)

  (use-package cdlatex
    :defer t
    :ensure t
    :init
    (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
    (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex))

  (use-package org
    :defer t
    :ensure t
    :custom
    (org-log-done t)
    (org-startup-indented t)
    (org-confirm-babel-evaluate nil)
    (org-src-fontify-natively t)
    (org-src-tab-acts-natively t)
    (org-hide-emphasis-markers t)
    :config
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((latex  . t)
       (R      . t)
       (C      . t)
       (python . t)
       (java   . t)
       (shell  . t))))

  (setq org-publish-project-alist
        '(("jeffpollock9"
           :base-directory "~/workspace/github.io/org"
           :base-extension "org"
           :publishing-directory "~/workspace/github.io/_posts/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :html-extension "html"
           :body-only t)))

  (use-package ox-twbs
    :ensure t)

  (use-package dashboard
    :ensure t
    :config
    (setq dashboard-items '((recents   . 5)
                            (bookmarks . 5)))
    (dashboard-setup-startup-hook))

  (use-package expand-region
    :ensure t
    :commands er/expand-region
    :bind ("M-p" . er/expand-region))

  (use-package dired
    :bind
    (:map dired-mode-map ("C-c C-e" . wdired-change-to-wdired-mode))
    :init
    (setq dired-listing-switches "-alh"
          dired-dwim-target t))

  (use-package htmlize
    :ensure t
    :defer t)

  (use-package zenburn-theme
    :ensure t
    :init
    (load-theme 'zenburn t))

  (use-package smart-mode-line
    :ensure t
    :init (sml/setup))

  (use-package cuda-mode
    :ensure t
    :defer t)

  (use-package clang-format
    :ensure t
    :defer t)

  (use-package stan-snippets
    :ensure t
    :defer t)

  (use-package stan-mode
    :defer t
    :ensure t)

  (use-package helm
    :ensure t
    :bind
    ("M-x" . helm-M-x)
    ("C-x C-f" . helm-find-files))

  (use-package helm-ag
    :ensure t
    :bind
    ("C-x C-g" . helm-do-ag))

  (use-package swiper
    :ensure t
    :diminish
    :bind
    ("C-s" . swiper))

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
    :config
    (global-undo-tree-mode))

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

  (use-package magit-todos
    :ensure t
    :init (magit-todos-mode))

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

  (use-package yasnippet-snippets
    :defer t
    :ensure t)

  (use-package yasnippet
    :defer t
    :ensure t
    :init
    (yas-global-mode)
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map (kbd "C-c TAB") 'yas-expand)
    :config (yas-reload-all))

  (use-package ess
    :ensure t
    :defer t
    :init
    (setq ess-smart-S-assign-key nil)
    (defun magrittr-pipe ()
      "insert %>%"
      (interactive)
      (just-one-space 1)
      (insert "%>%")
      (just-one-space 1))
    :bind
    (("C-=" . ess-insert-assign)
     :map ess-mode-map ("C->" . magrittr-pipe)
     :map inferior-ess-mode-map ("C->" . magrittr-pipe)))

  (use-package julia-mode
    :defer t
    :ensure t)

  (use-package treemacs
    :ensure t
    :config
    (setq treemacs-width 35
          treemacs-show-hidden-files nil
          treemacs-collapse-dirs 3
          treemacs-file-event-delay 5000)
    :bind
    (:map global-map ("M-0" . treemacs-select-window)))

  (use-package cmake-mode
    :ensure t
    :init (setq cmake-tab-width 4))

  (use-package cmake-font-lock
    :ensure t
    :hook cmake-mode-hook)

  (use-package company-quickhelp
    :ensure t
    :config
    (eval-after-load 'company
      '(define-key company-active-map (kbd "C-c C-d") #'company-quickhelp-manual-begin)))

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
    :mode ("\\.md$" . markdown-mode))

  (use-package yaml-mode
    :ensure t
    :mode (("\\.yml$'" . yaml-mode)
           ("\\.yaml$'" . yaml-mode)))

  (use-package dockerfile-mode
    :ensure t
    :mode ("Dockerfile" . dockerfile-mode))

  (use-package elpy
    :ensure t
    :diminish
    :init
    (elpy-enable)
    (delete `elpy-module-highlight-indentation elpy-modules)
    :bind
    (:map elpy-mode-map ("C-b" . elpy-autopep8-fix-code))
    :config
    (pyvenv-workon "pymacs"))

  (use-package duplicate-thing
    :ensure t
    :bind
    ("C-d" . duplicate-thing))

  (use-package doxymacs
    :load-path "~/.emacs.d/builds/doxymacs/install/share/emacs/site-lisp"
    :diminish
    :init
    (add-hook 'c-mode-common-hook 'doxymacs-mode)
    (defun my-doxymacs-font-lock-hook ()
      (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
          (doxymacs-font-lock)))
    (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
    (setq doxymacs-doxygen-style "C++")
    (setq doxymacs-command-character "\\"))

  ;; ispc
  (add-to-list 'auto-mode-alist '(".ispc$" . c++-mode))
  (add-to-list 'auto-mode-alist '(".isph$" . c++-mode))

  (use-package tex
    :ensure auctex
    :defer t)

  (use-package poly-R
    :ensure t
    :mode ("\\.Rmd$" . poly-markdown+r-mode))

  (use-package flymake
    :diminish
    :ensure t)

  (use-package flycheck
    :diminish
    :ensure t
    :init (global-flycheck-mode))

  (use-package flycheck-yamllint
    :ensure t
    :mode (("\\.yml$'" . yaml-mode)
           ("\\.yaml$'" . yaml-mode))
    :init
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))
    (add-hook 'yaml-mode-hook 'flycheck-mode))

  (use-package ansi-color
    :ensure t
    :init
    (defun colorize-compilation-buffer ()
      (read-only-mode)
      (ansi-color-apply-on-region compilation-filter-start (point))
      (read-only-mode))
    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

  (use-package lsp-ui
    :ensure t
    :init
    (setq lsp-ui-sideline-show-hover nil
          lsp-ui-sideline-show-symbol t))

  (use-package lsp-mode
    :ensure t
    :commands lsp
    :diminish
    :config (require 'lsp-clients)
    :init
    (add-hook 'lsp-mode-hook 'lsp-ui-mode))

  (use-package ccls
    :ensure t
    :bind
    (:map c++-mode-map
          ("C-b" . clang-format-buffer)
          ("C-d" . duplicate-thing))
    :init
    (add-hook 'c++-mode-hook (lambda () (require 'ccls) (lsp))))

  (use-package company-lsp
    :ensure t
    :init
    (push 'company-lsp company-backends)
    (setq company-transformers nil
          company-lsp-async t
          company-lsp-cache-candidates nil))

  (use-package company-shell
    :ensure t
    :init
    (add-to-list 'company-backends 'company-shell))

  ;; diminish some minor modes
  (add-hook 'ess-r-mode-hook (lambda () (diminish 'ess-r-package-mode)))
  (add-hook 'auto-revert-mode-hook (lambda () (diminish 'auto-revert-mode)))
  (add-hook 'page-break-lines-mode-hook (lambda () (diminish 'page-break-lines-mode)))
  )

;;; init.el ends here
