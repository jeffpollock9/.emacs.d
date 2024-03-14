;;; init.el

;;; code:

;; TOOD: take ideas from https://github.com/malb/emacs.d/blob/master/malb.org
;; TODO: take ideas from https://github.com/seagle0128/.emacs.d

(let ((file-name-handler-alist nil)
      (gc-cons-threshold (* 1024 1024 100)))

  (setq byte-compile-warnings '(cl-functions))
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file)

  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa"     . "http://melpa.org/packages/"))
  ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("gnu"       . "http://elpa.gnu.org/packages/"))
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (eval-when-compile (require 'use-package))

  (fset 'yes-or-no-p 'y-or-n-p)

  (xterm-mouse-mode 1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode 0)
  (show-paren-mode 1)
  (global-auto-revert-mode 1)

  (setq use-file-dialog nil
        use-dialog-box nil
        column-number-mode t
        inhibit-startup-screen t
        inhibit-startup-echo-area-message t
        indent-line-function 'insert-tab
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)

  (setq ispell-program-name "hunspell"
        ispell-local-dictionary "en_GB")

  (when (boundp 'ns-pop-up-frames)
    (setq ns-pop-up-frames nil))

  (when (boundp 'x-gtk-use-system-tooltips)
    (setq x-gtk-use-system-tooltips nil))

  (setq-default fill-column 88
                show-paren-delay 0
                major-mode 'text-mode
                indent-tabs-mode nil
                tab-width 4)

  (define-minor-mode resize-mode
    "Toggle resizing of current window"
    :init-value nil
    :lighter " resize"
    :keymap
    '(([left]   . shrink-window-horizontally)
      ([right]  . enlarge-window-horizontally)
      ([up]     . enlarge-window)
      ([down]   . shrink-window)
      ([return] . resize-mode)))

  (define-minor-mode string-inflection-mode
    "Toggle changing string case mode"
    :init-value nil
    :lighter " string-inflection-mode"
    :keymap
    '(("s"      . string-inflection-underscore)
      ("u"      . string-inflection-upcase)
      ("l"      . string-inflection-lisp)
      ("c"      . string-inflection-lower-camelcase)
      ("p"      . string-inflection-camelcase)
      ([return] . string-inflection-mode)))

  (global-set-key (kbd "C-#") 'comment-or-uncomment-region)
  (global-set-key (kbd "C-u") '(lambda () (interactive) (kill-line 0)))
  (global-set-key (kbd "C-l") 'comint-clear-buffer)
  (global-set-key (kbd "C-c t") 'toggle-truncate-lines)
  (global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
  (global-set-key (kbd "C-c r") 'resize-mode)
  (global-set-key (kbd "C-c c") 'string-inflection-mode)

  (defun my-c-mode-common-hook ()
    (c-set-offset 'comment-intro 0)
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'innamespace 0)

    (setq c++-tab-always-indent t
          c-basic-offset 4
          c-indent-level 4
          c-file-style "stroustrup"
          tab-stop-list (number-sequence 2 200 2)
          tab-width 4
          indent-tabs-mode nil))

  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


  (setq treesit-language-source-alist
        '(
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          ))

  (dolist (lang treesit-language-source-alist)
    (unless (treesit-language-available-p (car lang))
      (treesit-install-language-grammar (car lang))))

  (setq major-mode-remap-alist
        '(
          (yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (c++-mode . c++-ts-mode)
          (c-mode . c-ts-mode)
          ))

  (add-hook 'python-ts-mode-hook 'eglot-ensure)

  (use-package conf-mode
    :ensure nil
    :hook (conf-mode . (lambda () (electric-indent-local-mode -1))))

  (use-package pyvenv
    :ensure t
    :bind
    (:map pyvenv-mode-map
          ("C-c q" . pyvenv-restart-python)
          ("C-c o" . pyvenv-activate))
    :init
    (pyvenv-mode))

  (use-package json-mode
    :ensure t)

  (use-package diminish
    :ensure t
    :diminish abbrev-mode)

  (use-package crontab-mode
    :ensure t)

  (use-package csv-mode
    :ensure t)

  (use-package bind-key
    :ensure t)

  (use-package ibuffer
    :bind ("C-x C-b" . ibuffer)
    :init
    (setq ibuffer-expert t
          ibuffer-show-empty-filter-groups nil
          ibuffer-saved-filter-groups
          '(("groups"
             ("Emacs"          (mode . emacs-lisp-mode))
             ("Org"            (mode . org-mode))
             ("Python"         (mode . python-mode))
             ("Python-console" (mode . inferior-python-mode))
             ("R"              (mode . ess-r-mode))
             ("R-console"      (mode . inferior-ess-r-mode))
             ("Stan"           (mode . stan-mode))
             ("C++"            (or (mode . c-mode)
                                   (mode . c++-mode)))
             ("Cmake"          (mode . cmake-mode))
             ("Make"           (name . "[mM]akefile"))
             ("Magit"          (name . "\*magit"))
             ("Markdown"       (mode . markdown-mode))
             ("Dired"          (mode . dired-mode))
             ("Init"           (or (mode . dashboard-mode)
                                   (name . "\*scratch\*")))
             )))

    (add-hook 'ibuffer-mode-hook
              '(lambda () (ibuffer-switch-to-saved-filter-groups "groups"))))

  (use-package which-key
    :ensure t
    :diminish
    :config
    (which-key-mode))

  (use-package helpful
    :ensure t
    :bind
    ("C-h f" . helpful-callable)
    ("C-h v" . helpful-variable)
    ("C-h k" . helpful-key))

  (use-package cdlatex
    :defer t
    :ensure t
    :init
    (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
    (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex))

  (use-package pdf-tools
    :ensure t
    :config
    (pdf-tools-install)
    (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

  (use-package jupyter
    :defer t
    :ensure t
    :init
    (setq jupyter-repl-echo-eval-p t)
    (use-package ob-jupyter
      :defer t
      :commands (org-babel-execute:jupyter-python)
      :bind
      (:map jupyter-repl-mode-map
            ("C-l" . jupyter-repl-clear-cells)
            ("C-<up>" . jupyter-repl-history-previous)
            ("C-<down>" . jupyter-repl-history-next))))

  (use-package org
    :defer t
    :ensure t
    :custom
    (org-log-done t)
    (org-startup-indented t)
    (org-confirm-babel-evaluate nil)
    (org-src-fontify-natively t)
    (org-src-tab-acts-natively t)
    :config
    ;; TODO: some sort of default setup file which adds this
    ;;       https://www.reddit.com/r/emacs/comments/8gnsm2/orgmode_default_document
    ;; (setq org-html-head-extra "<style>pre { background-color: #3f3f3f; color: #dcdccc; }</style>")
    (setq org-highlight-latex-and-related '(latex script entities))
    (plist-put org-format-latex-options :scale 1.75)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((latex   . t)
       (R       . t)
       (C       . t)
       (python  . t)
       (jupyter . t)
       (java    . t)
       (shell   . t))))

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

  (use-package ox-gfm
    :ensure t)

  (use-package org-bullets
    :ensure t
    :diminish
    :hook (org-mode . org-bullets-mode))

  (use-package org-re-reveal
    :defer t
    :ensure t
    :config
    (setq org-re-reveal-root "file:///home/jeff/workspace/reveal.js"))

  (use-package dashboard
    :ensure t
    :config
    (setq dashboard-items '((recents   . 20)
                            (bookmarks . 5)))
    (dashboard-setup-startup-hook))

  (use-package expand-region
    :ensure t
    :commands er/expand-region
    :bind ("M-p" . er/expand-region))

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

  (use-package clang-format
    :ensure t
    :defer t
    :bind
    (:map c++-mode-map ("C-c b" . clang-format-buffer))
    (:map c-mode-map ("C-c b" . clang-format-buffer)))

  (use-package cuda-mode
    :ensure t
    :defer t
    :bind
    (:map cuda-mode-map ("C-c b" . clang-format-buffer)))

  (use-package stan-mode
    :defer t
    :ensure t
    :mode ("\\.stan\\'" . stan-mode)
    :hook (stan-mode . stan-mode-setup)
    :config
    (setq stan-indentation-offset 4))


  (use-package company-stan
    :defer t
    :ensure t
    :hook (stan-mode . company-stan-setup))

  (use-package eldoc
    :ensure t
    :defer t
    :diminish)

  (use-package eldoc-stan
    :ensure t
    :defer t
    :hook (stan-mode . eldoc-stan-setup))

  (use-package flycheck-stan
    :ensure t
    :defer t
    :hook ((stan-mode . flycheck-stan-stanc2-setup)
           (stan-mode . flycheck-stan-stanc3-setup))
    :config
    (setq flycheck-stanc-executable nil
          flycheck-stanc3-executable nil))

  (use-package stan-snippets
    :ensure t
    :defer t
    :hook (stan-mode . stan-snippets-initialize))

  (use-package helm-icons
    :ensure t
    :config (helm-icons-enable))

  (use-package helm
    :after helm-icons
    :ensure t
    :init
    (setq helm-always-two-windows nil
          helm-display-buffer-default-height 18
          helm-split-window-inside-p t)
    :bind
    ("M-x"     . helm-M-x)
    ("C-x b"   . helm-mini)
    ("C-s"     . helm-occur)
    ("C-c h"   . helm-comint-input-ring)
    ("C-x C-f" . helm-find-files))

  (use-package helm-ag
    :ensure t
    :bind
    ("C-c g" . helm-do-ag)
    ("C-c v" . helm-do-ag-project-root))

  (use-package helm-tramp
    :ensure t
    :bind
    ("C-c s" . helm-tramp))

  (use-package tramp
    :ensure t
    :init
    (setq tramp-default-method "ssh"))

  (use-package smartparens
    :ensure t
    :diminish
    :init (smartparens-global-mode))

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

  (use-package string-inflection
    :ensure t)

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

  (use-package yasnippet-snippets
    :defer t
    :ensure t)

  (use-package yasnippet
    :diminish
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
    (setq ess-eval-visibly 'nowait)
    (defun magrittr-pipe ()
      "insert %>%"
      (interactive)
      (just-one-space 1)
      (insert "%>%")
      (just-one-space 1))
    (defun dev-off ()
      "close plot windows"
      (interactive)
      (let ((proc (ess-get-process)))
        (ess-send-string proc "dev.off()")))
    :bind
    (("C-=" . ess-insert-assign)
     ("C-c f" . dev-off)
     ("C->" . magrittr-pipe)))

  (use-package julia-mode
    :defer t
    :ensure t)

  (use-package dired
    :bind
    (:map dired-mode-map ("C-c C-e" . wdired-change-to-wdired-mode))
    :init
    (setq dired-listing-switches "-algh"
          dired-dwim-target t
          dired-omit-mode t))

  (use-package dired-filter
    :diminish
    :ensure t)

  ;; (use-package dired+
  ;;   :load-path "~/.emacs.d/emacswiki"
  ;;   :init
  ;;   (setq diredp-hide-details-initially-flag nil))

  (use-package vundo
    :ensure t
    :bind
    (:map global-map ("C-c u" . vundo)))

  (use-package treemacs
    :ensure t
    :config
    (setq treemacs-width 35
          treemacs-show-hidden-files nil
          treemacs-collapse-dirs 3
          treemacs-file-event-delay 5000)
    :bind
    (:map global-map ("M-0" . treemacs-select-window)))

  (use-package treemacs-icons-dired
    :ensure t
    :after (dired dired)
    :config
    (treemacs-icons-dired-mode)
    (treemacs-resize-icons 18))

  (use-package cmake-mode
    :ensure t
    :init (setq cmake-tab-width 4))

  (use-package cmake-font-lock
    :ensure t
    :hook cmake-mode-hook)

  (use-package markdown-mode
    :ensure t
    :mode ("\\.md$" . markdown-mode))

  (use-package markdown-toc
    :ensure t)

  (use-package yaml-mode
    :ensure t
    :mode (("\\.yml$'" . yaml-mode)
           ("\\.yaml$'" . yaml-mode)))

  (use-package dockerfile-mode
    :ensure t
    :mode ("Dockerfile" . dockerfile-mode))

  (use-package python-pytest
    :after python
    :ensure t
    :bind (:map python-mode-map ("C-c p" . python-pytest-dispatch)))

  (use-package pip-requirements
    :ensure t
    :hook (pip-requirements-mode . (lambda () (electric-indent-local-mode -1))))

  (use-package elpy
    :after python
    :ensure t
    :init
    (require 'elpy)
    :bind (:map python-mode-map
                ("C-c C-c" . elpy-shell-send-region-or-buffer)
                ("<C-return>" . elpy-shell-send-statement-and-step)))

  (use-package sphinx-doc
    :ensure t
    :diminish
    :hook (python-mode . sphinx-doc-mode))

  (use-package python-black
    :demand t
    :after python)

  (use-package python-isort
    :ensure t
    :after python)

  (defun my-python-format ()
    "black + isort"
    (interactive)
    (python-black-buffer)
    (python-isort-buffer)
    )

  (use-package python
    :ensure t
    :bind (:map python-ts-mode-map ("C-c b" . my-python-format)))

  (use-package duplicate-thing
    :ensure t
    :bind
    ("C-c d" . duplicate-thing))

  (use-package vterm
    :load-path "~/.emacs.d/builds/emacs-libvterm")

  ;; (use-package doxymacs
  ;;   :load-path "~/.emacs.d/builds/doxymacs/install/share/emacs/site-lisp"
  ;;   :diminish
  ;;   :init
  ;;   (add-hook 'c-mode-common-hook 'doxymacs-mode)
  ;;   (defun my-doxymacs-font-lock-hook ()
  ;;     (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
  ;;         (doxymacs-font-lock)))
  ;;   (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
  ;; (setq doxymacs-doxygen-style "C++"
  ;;       doxymacs-command-character "\\")
  ;; :config
  ;; (unbind-key "C-c d" doxymacs-mode-map))

  ;; ispc
  (add-to-list 'auto-mode-alist '("\\.ispc$" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.isph$" . c++-mode))

  (use-package tex
    :ensure auctex
    :defer t)

  (use-package sqlformat
    :ensure t
    :defer t
    :config
    (setq sqlformat-command 'sqlfluff)
    :bind
    (:map sql-mode-map ("C-c b" . sqlformat-buffer)))

  (use-package poly-R
    :ensure t)

  (use-package flymake
    :diminish
    :ensure t)

  (use-package flycheck
    :diminish
    :ensure t
    :init
    (global-flycheck-mode))

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

  (use-package projectile
    :ensure t
    :diminish
    :init
    (projectile-mode +1)
    (setq projectile-switch-project-action 'projectile-dired)
    :bind
    (:map projectile-mode-map
          ("C-c m" . projectile-command-map)))

  (use-package helm-projectile
    :ensure t
    :init
    (helm-projectile-on))

  ;; (use-package lsp-mode
  ;;   :ensure t
  ;;   :diminish
  ;;   :config
  ;;   (setq lsp-enable-symbol-highlighting nil
  ;;         lsp-headerline-breadcrumb-enable nil)
  ;;   :bind
  ;;   (:map lsp-mode-map
  ;;         ;; ("C-c b" . lsp-format-buffer)
  ;;         ("C-c i" . lsp-describe-thing-at-point)))

  ;; (use-package lsp-pyright
  ;;   :ensure t
  ;;   :after python
  ;;   :hook (python-mode . (lambda ()
  ;;                          (require 'lsp-pyright)
  ;;                          (lsp))))

  (use-package ccls
    :ensure t
    :defer t
    :hook ((c++-mode cuda-mode) . (lambda () (require 'ccls) (lsp))))

  (use-package company
    :diminish
    :ensure t
    :init (global-company-mode)
    :bind ("<C-tab>" . company-complete)
    :config
    (setq company-tooltip-align-annotations t
          company-tooltip-limit 20
          company-show-numbers t
          company-idle-delay nil
          company-require-match nil))

  (use-package company-shell
    :ensure t
    :init
    (add-to-list 'company-backends 'company-shell))

  ;; diminish some minor modes
  (add-hook 'ess-r-mode-hook (lambda () (diminish 'ess-r-package-mode)))
  (add-hook 'auto-revert-mode-hook (lambda () (diminish 'auto-revert-mode)))
  (add-hook 'page-break-lines-mode-hook (lambda () (diminish 'page-break-lines-mode)))
  (add-hook 'org-indent-mode-hook (lambda () (diminish 'org-indent-mode)))
  (add-hook 'org-cdlatex-mode-hook (lambda () (diminish 'org-cdlatex-mode)))
  (add-hook 'yas-minor-mode-hook (lambda () (diminish 'yas-minor-mode)))
  (add-hook 'yas-major-mode-hook (lambda () (diminish 'yas-major-mode)))
  )

;;; init.el ends here
