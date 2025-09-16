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

  (setenv "PYTHONIOENCODING" "utf-8")
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-language-environment 'utf-8)
  (set-selection-coding-system 'utf-8)

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
  (global-set-key (kbd "C-l") 'comint-clear-buffer)
  (global-set-key (kbd "C-c w") 'delete-trailing-whitespace)

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
          indent-tabs-mode nil)

    )

  (add-hook 'c-ts-mode-common-hook 'my-c-mode-common-hook)
  (add-hook 'c++-ts-mode-common-hook 'my-c-mode-common-hook)
  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
  (add-hook 'c++-mode-common-hook 'my-c-mode-common-hook)

  (use-package treesit-auto
    :ensure t
    :custom
    (treesit-auto-install t)
    :config
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode))

  (add-hook 'cmake-ts-mode-hook 'eglot-ensure)
  (add-hook 'python-ts-mode-hook 'eglot-ensure)
  (add-hook 'c++-ts-mode-hook 'eglot-ensure)

  ;; Emacs minibuffer configurations.
  (use-package emacs
    :custom
    ;; Support opening new minibuffers from inside existing minibuffers.
    (enable-recursive-minibuffers t)
    ;; Hide commands in M-x which do not work in the current mode.  Vertico
    ;; commands are hidden in normal buffers. This setting is useful beyond
    ;; Vertico.
    (read-extended-command-predicate #'command-completion-default-include-p)
    ;; Do not allow the cursor in the minibuffer prompt
    (minibuffer-prompt-properties
     '(read-only t cursor-intangible t face minibuffer-prompt)))

  (use-package envrc
    :ensure t
    :diminish
    :hook (after-init . envrc-global-mode))

  (use-package markdown-mode
    :ensure t
    :mode ("\\.md$" . markdown-mode))

  (use-package markdown-toc
    :ensure t)

  (use-package eglot
    :after markdown-mode
    :ensure t
    :diminish
    :bind
    (:map eglot-mode-map
          ("C-c r" . eglot-rename)
          ("C-c f" . eglot-code-action-quickfix)
          ("C-c b" . eglot-format-buffer))
    :config
    ;; c++
    (add-to-list 'eglot-server-programs
                 '((c++-mode c++-ts-mode) . ("clangd"
                                            "--background-index"
                                            "--clang-tidy"
                                            "--completion-style=detailed"
                                            "--function-arg-placeholders"
                                            "--header-insertion=iwyu")))
    ;; python
    (add-to-list 'eglot-server-programs '((python-mode python-ts-mode)
                                          "basedpyright-langserver" "--stdio"))
    (setq-default
       eglot-workspace-configuration
       '(:basedpyright (
           :typeCheckingMode "basic"
         )
         :basedpyright.analysis (
           :diagnosticSeverityOverrides (
             :reportUnusedCallResult "none"
             :reportUnknownMemberType "none"
           )
           :inlayHints (
             :variableTypes :json-false
           )
         )))
    )

  (use-package eglot-booster
	:after eglot
    :diminish
    :vc (eglot-booster :url "https://github.com/jdtsmith/eglot-booster")
	:config	(eglot-booster-mode))

  (use-package eldoc-box
	:after eglot
    :ensure t
    :bind
    (:map eglot-mode-map
          ("C-c h" . eldoc-box-help-at-point)))

  (use-package pyvenv
    :ensure t
    :bind
    (:map pyvenv-mode-map
          ("C-c q" . pyvenv-restart-python)
          ("C-c o" . pyvenv-activate))
    :init
    (pyvenv-mode))

  (use-package conf-mode
    :ensure nil
    :hook (conf-mode . (lambda () (electric-indent-local-mode -1))))

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
       ;;(jupyter . t)
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

  (use-package nerd-icons
    :ensure t
    :custom
    (nerd-icons-font-family "Symbols Nerd Font Mono"))

  (use-package dashboard
    :ensure t
    :after nerd-icons
    :config
    (setq dashboard-projects-backend 'projectile)
    (setq dashboard-items '((projects . 5) (recents . 20) (bookmarks . 51)))
    (setq dashboard-display-icons-p t)
    (setq dashboard-icon-type 'nerd-icons)
    (dashboard-setup-startup-hook))

  (use-package expand-region
    :ensure t
    :commands er/expand-region
    :bind ("M-p" . er/expand-region))

  (use-package htmlize
    :ensure t
    :defer t)

  (use-package modus-themes
    :ensure t
    :init
    (load-theme 'modus-vivendi-tinted t))

  (use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode 1))

  (use-package cuda-mode
    :ensure t
    :defer t)

  (use-package stan-mode
    :defer t
    :ensure t
    :mode ("\\.stan\\'" . stan-mode)
    :hook (stan-mode . stan-mode-setup)
    :config
    (setq stan-indentation-offset 4))

  (use-package eldoc
    :ensure t
    :defer t
    :diminish
    :config
    (defvar rb--eldoc-html-patterns
      '(("&nbsp;" " ")
        ("&lt;" "<")
        ("&gt;" ">")
        ("&amp;" "&")
        ("&quot;" "\"")
        ("&apos;" "'"))
      "List of (PATTERN . REPLACEMENT) to replace in eldoc output.")

    (defun rb--string-replace-all (patterns in-string)
      "Replace all cars from PATTERNS in IN-STRING with their pair."
      (mapc (lambda (pattern-pair)
              (setq in-string
                    (string-replace (car pattern-pair) (cadr pattern-pair) in-string)))
            patterns)
      in-string)

    (defun rb--eldoc-preprocess (orig-fun &rest args)
      "Preprocess the docs to be displayed by eldoc to replace HTML escapes."
      (let ((doc (car args)))
        ;; The first argument is a list of (STRING :KEY VALUE ...) entries
        ;; we replace the text in each such string
        ;; see docstring of `eldoc-display-functions'
        (when (listp doc)
          (setq doc (mapcar
                     (lambda (doc) (cons
                                    (rb--string-replace-all rb--eldoc-html-patterns (car doc))
                                    (cdr doc)))
                     doc
                     ))
          )
        (apply orig-fun (cons doc (cdr args)))))

    (advice-add 'eldoc-display-in-buffer :around #'rb--eldoc-preprocess)
    )

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

  (use-package c-ts-mode
    :ensure t
    :config
    (setq c-ts-mode-indent-offset 4))

  (use-package cmake-ts-mode
    :ensure t
    :config
    (setq cmake-ts-mode-indent-offset 4))

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

  (use-package elpy
    :after python
    :ensure t
    :init
    (require 'elpy)
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt")
    :bind (:map python-ts-mode-map
                ("C-c C-c" . elpy-shell-send-region-or-buffer)
                ("C-c l" . elpy-shell-send-statement-and-step)))

  (use-package sphinx-doc
    :ensure t
    :diminish
    :hook (python-mode . sphinx-doc-mode))

  (use-package python
    :ensure t)

  (use-package duplicate-thing
    :ensure t
    :bind
    ("C-c d" . duplicate-thing))

  (use-package vterm
    :load-path "~/.emacs.d/builds/emacs-libvterm")

  (use-package cython-mode
    :load-path "~/.emacs.d/lisp")

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

  (use-package reformatter
    :ensure t
    :config
    (reformatter-define ruff-format
      :program "ruff"
      :args `("format" "--stdin-filename" ,buffer-file-name "-"))
    (reformatter-define ruff-check
      :program "ruff"
      :args `("check" "--fix" "--stdin-filename" ,buffer-file-name "-"))
    )

  (defun ruff-all ()
    "ruff check --fix & ruff format"
    (interactive)
    (ruff-check-buffer)
    (ruff-format-buffer))

  (defun my-eglot-format-advice (orig-fun &rest args)
    "Use ruff for Python formatting."
    (if (eq major-mode 'python-ts-mode)
        (ruff-all)
      (apply orig-fun args)))

  (advice-add 'eglot-format-buffer :around #'my-eglot-format-advice)

  (use-package tex
    :ensure auctex
    :defer t)

  (use-package sqlformat
    :ensure t
    :defer t
    :config
    (setq sqlformat-command 'sqlfluff)
    (setq sqlformat-args '("--dialect" "clickhouse"))
    :bind
    (:map sql-mode-map ("C-c b" . sqlformat-buffer)))

  (use-package poly-R
    :ensure t)

  (use-package flymake
    :diminish
    :ensure t)

  (use-package flycheck
    :diminish
    :ensure t)

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

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

  (use-package projectile
    :ensure t
    :diminish
    :init
    (projectile-mode +1)
    (setq projectile-switch-project-action 'projectile-dired)
    :bind
    (:map projectile-mode-map
          ("C-c m" . projectile-command-map)))

  (use-package fussy
    :ensure t
    :after eglot
    :config
    (fussy-setup)
    (fussy-eglot-setup))

  (use-package fzf-native
    :ensure t
    :vc (fzf-native :url "https://github.com/dangduc/fzf-native")
    :config
    (setq fussy-score-fn 'fussy-fzf-native-score)
    (fzf-native-load-dyn))

  (advice-add 'corfu--capf-wrapper :before 'fussy-wipe-cache)

  (add-hook 'corfu-mode-hook
            (lambda ()
              (setq-local fussy-max-candidate-limit 5000
                          fussy-default-regex-fn 'fussy-pattern-first-letter
                          fussy-prefer-prefix nil)))

  (use-package corfu
    :ensure t
    :custom
    (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
    (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
    (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
    (corfu-preview-current nil)    ;; Disable current candidate preview
    (corfu-preselect 'first)
    (corfu-on-exact-match nil)     ;; Configure handling of exact matches
    :init
    (global-corfu-mode)
    (corfu-history-mode)
    (corfu-popupinfo-mode)
    (corfu-indexed-mode)
    (dotimes (i 10)
      (define-key corfu-mode-map
                  (kbd (format "M-%s" i))
                  (kbd (format "C-%s <tab>" i))))
    )

  (use-package vertico
    :ensure t
    :custom
    (vertico-scroll-margin 0) ;; Different scroll margin
    (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
    (vertico-resize nil)
    :init
    (vertico-mode))

  (use-package marginalia
    :ensure t
    :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
    :init
    (marginalia-mode))

  (use-package savehist
    :ensure t
    :init
    (savehist-mode))

  (use-package orderless
    :ensure t
    :custom
    ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
    (orderless-component-separator #'orderless-escapable-split-on-space)
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles partial-completion)))))

  (use-package wgrep
    :ensure t)

  (use-package embark
    :ensure t
    :bind
    (("M-." . embark-act)
     ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
    :init
    (setq prefix-help-command #'embark-prefix-help-command)
    :config
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none)))))

  (use-package embark-consult
    :ensure t
    :hook
    (embark-collect-mode . consult-preview-at-point-mode))

  (use-package consult
    :ensure t
    :bind (;; C-c bindings in `mode-specific-map'
           ("C-c M-x" . consult-mode-command)
           ("C-c h" . consult-history)
           ("C-c k" . consult-kmacro)
           ("C-c m" . consult-man)
           ("C-c i" . consult-info)
           ([remap Info-search] . consult-info)
           ;; C-x bindings in `ctl-x-map'
           ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
           ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
           ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
           ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
           ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
           ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
           ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
           ;; Custom M-# bindings for fast register access
           ("M-#" . consult-register-load)
           ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
           ("C-M-#" . consult-register)
           ;; Other custom bindings
           ("M-y" . consult-yank-pop)                ;; orig. yank-pop
           ;; M-g bindings in `goto-map'
           ("M-g e" . consult-compile-error)
           ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
           ("M-g g" . consult-goto-line)             ;; orig. goto-line
           ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
           ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
           ("M-g m" . consult-mark)
           ("M-g k" . consult-global-mark)
           ("M-g i" . consult-imenu)
           ("M-g I" . consult-imenu-multi)
           ;; M-s bindings in `search-map'
           ("M-s d" . consult-find)                  ;; Alternative: consult-fd
           ("M-s c" . consult-locate)
           ("M-s g" . consult-grep)
           ("M-s G" . consult-git-grep)
           ("M-s r" . consult-ripgrep)
           ("M-s l" . consult-line)
           ("M-s L" . consult-line-multi)
           ("M-s k" . consult-keep-lines)
           ("M-s u" . consult-focus-lines)
           ;; Isearch integration
           ("M-s e" . consult-isearch-history)
           :map isearch-mode-map
           ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
           ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
           ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
           ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
           ;; Minibuffer history
           :map minibuffer-local-map
           ("M-s" . consult-history)                 ;; orig. next-matching-history-element
           ("M-r" . consult-history))                ;; orig. previous-matching-history-element
    ;; Enable automatic preview at point in the *Completions* buffer. This is
    ;; relevant when you use the default completion UI.
    :hook (completion-list-mode . consult-preview-at-point-mode)
    ;; The :init configuration is always executed (Not lazy)
    :init
    ;; Tweak the register preview for `consult-register-load',
    ;; `consult-register-store' and the built-in commands.  This improves the
    ;; register formatting, adds thin separator lines, register sorting and hides
    ;; the window mode line.
    (advice-add #'register-preview :override #'consult-register-window)
    (setq register-preview-delay 0.5)
    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)
    ;; Configure other variables and modes in the :config section,
    ;; after lazily loading the package.
    :config
    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key "M-.")
    ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
     consult-theme :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep consult-man
     consult-bookmark consult-recent-file consult-xref
     consult--source-bookmark consult--source-file-register
     consult--source-recent-file consult--source-project-recent-file
     ;; :preview-key "M-."
     :preview-key '(:debounce 0.4 any))
    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; "C-+"
    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
    )

  (use-package helpful
    :ensure t
    :bind
    ("C-h f" . helpful-callable)
    ("C-h v" . helpful-variable)
    ("C-h k" . helpful-key))

  (use-package kind-icon
    :ensure t
    :after corfu
    :custom
    (kind-icon-blend-background t)
    (kind-icon-default-face 'corfu-default)
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

  ;; diminish some minor modes
  (add-hook 'ess-r-mode-hook (lambda () (diminish 'ess-r-package-mode)))
  (add-hook 'auto-revert-mode-hook (lambda () (diminish 'auto-revert-mode)))
  (add-hook 'page-break-lines-mode-hook (lambda () (diminish 'page-break-lines-mode)))
  (add-hook 'org-indent-mode-hook (lambda () (diminish 'org-indent-mode)))
  (add-hook 'org-cdlatex-mode-hook (lambda () (diminish 'org-cdlatex-mode)))
  (add-hook 'yas-minor-mode-hook (lambda () (diminish 'yas-minor-mode)))
  (add-hook 'yas-major-mode-hook (lambda () (diminish 'yas-majormode)))
  )

;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
