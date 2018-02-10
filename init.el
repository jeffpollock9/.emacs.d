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

(eval-when-compile
  (require 'use-package))

(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq column-number-mode t)
(setq-default fill-column 80)

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

(use-package diminish :ensure t)

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

(use-package ess-R-object-popup
  :ensure t
  :init (define-key ess-mode-map (kbd "C-c v") 'ess-R-object-popup))

(use-package neotree
  :ensure t
  :bind ([f8] . neotree-toggle)
  :init
  (setq neo-window-width 40)
  (setq neo-autorefresh nil)
  (neotree-show))

(use-package cmake-mode
  :ensure t
  :init (setq cmake-tab-width 4))

(use-package cmake-font-lock
  :ensure t
  :hook cmake-mode-hook)

(use-package company
  :ensure t
  :commands (global-company-mode)
  :init (global-company-mode t)
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

(use-package flycheck-yamllint
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode))
  :init
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))
  (add-hook 'yaml-mode-hook 'flycheck-mode))

(use-package elpy
  :ensure t
  :init
  (setq python-shell-interpreter "python3")
  (setq elpy-rpc-python-command "python3")
  (elpy-enable)
  :bind
  ("C-b" . elpy-autopep8-fix-code))

(use-package doxymacs
  :load-path "~/.emacs.d/builds/doxymacs/install/share/emacs/site-lisp"
  :init
  (add-hook 'c-mode-common-hook'doxymacs-mode)
  (defun my-doxymacs-font-lock-hook ()
    (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
	(doxymacs-font-lock)))
  (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
  (setq doxymacs-doxygen-style "C++")
  (setq doxymacs-command-character "\\"))

(use-package flycheck-rtags
  :load-path load-path "~/.emacs.d/builds/rtags/install/share/emacs/site-lisp/rtags")

(use-package rtags
  :load-path load-path "~/.emacs.d/builds/rtags/install/share/emacs/site-lisp/rtags"
  :init
  (defun my-cpp-setup ()
    (local-set-key (kbd "C-b") 'clang-format-buffer)
    (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
    (local-set-key (kbd "M-,") 'rtags-show-target-in-other-window)
    (local-set-key (kbd "M-i") 'rtags-include-file)
    (local-set-key (kbd "M-n") 'rtags-next-match)
    (local-set-key (kbd "M-p") 'rtags-previous-match)

    (rtags-enable-standard-keybindings)
    (setq rtags-autostart-diagnostics t)
    (rtags-diagnostics)
    (setq rtags-completions-enabled t)
    (push 'company-rtags company-backends)

    (flycheck-select-checker 'rtags)
    (setq-local flycheck-highlighting-mode nil)
    (setq-local flycheck-check-syntax-automatically nil))

  (add-hook 'c-mode-common-hook #'my-cpp-setup)
  (add-hook 'c++-mode-hook #'my-cpp-setup))

;; ispc
(add-to-list 'auto-mode-alist '(".ispc$" . c++-mode))
(add-to-list 'auto-mode-alist '(".isph$" . c++-mode))

(use-package polymode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode)))

(defun duplicate-line-or-region (&optional n)
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1))
                          (newline))))))
        (dotimes (i (abs (or n 1)))
          (insert text))))
    (if use-region nil
      (let ((pos (- (point) (line-beginning-position))))
        (if (> 0 n)
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(global-set-key (kbd "C-c d")  'duplicate-line-or-region)
