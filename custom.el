(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-dabbrev-downcase nil)
 '(company-idle-delay nil)
 '(company-minimum-prefix-length 2)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(company-show-numbers t)
 '(company-show-quick-access t)
 '(company-tooltip-limit 20)
 '(custom-safe-themes
   '("fc1275617f9c8d1c8351df9667d750a8e3da2658077cfdda2ca281a2ebc914e0"
     "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9"
     "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
     "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
     "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0"
     "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba"
     "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" default))
 '(delete-selection-mode 1)
 '(fci-rule-color "#383838")
 '(flycheck-lintr-linters
   "with_defaults(line_length_linter(88), object_length_linter(40))")
 '(helm-ff-skip-boring-files t)
 '(inhibit-startup-screen t)
 '(markdown-command "pandoc")
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-export-backends '(md odt latex icalendar html ascii))
 '(org-export-preserve-breaks t)
 '(package-selected-packages
   '(auctex buffer-move cdlatex clang-format claude-code compile-multi-embark
            compile-multi-nerd-icons consult-compile-multi corfu coterm crontab-mode
            csv-mode cuda-mode dashboard diminish dired-filter dockerfile-mode
            doom-modeline drag-stuff duplicate-thing eat eglot-booster eldoc-box
            eldoc-stan elpy embark-consult envrc expand-region fancy-compilation
            flycheck-stan flycheck-yamllint fussy fzf-native god-mode helpful iedit
            json-mode julia-mode jupyter kind-icon magit marginalia markdown-toc
            modus-themes multiple-cursors orderless org-bullets org-re-reveal ox-gfm
            ox-twbs pdf-tools poly-R projectile pulsar python-pytest rmsbolt smartparens
            smooth-scrolling sphinx-doc sqlformat stan-snippets string-inflection
            treemacs-icons-dired treesit-auto vertico vundo wgrep yaml-mode
            yasnippet-snippets))
 '(package-vc-selected-packages
   '((claude-code :url "https://github.com/stevemolitor/claude-code.el")
     (fzf-native :url "https://github.com/dangduc/fzf-native")
     (vc-use-package :vc-backend Git :url "https://github.com/slotThe/vc-use-package")
     (eglot-booster :url "https://github.com/jdtsmith/eglot-booster")))
 '(safe-local-variable-values
   '((compile-multi-config
      (t ("snipycpp::pytest" . "cd snipycpp && uv run --no-progress --reinstall pytest")
         ("snipycpp::pytest-benchmark"
          . "cd snipycpp && uv run --no-progress --reinstall pytest --benchmark-enable")
         ("snipycpp::clean" . "rm -rf snipycpp/build")
         ("snipycpp::cmake" . "cd snipycpp && cmake --preset release")
         ("snipycpp::catch-test"
          . "cd snipycpp/build/release && ninja && ./tests/test_runner")
         ("snipy::pytest" . "cd ../snipy && pytest")))
     (compile-multi-config
      (t ("snipycpp::pytest" . "cd snipycpp && uv run --no-progress --reinstall pytest")
         ("snipycpp::pytest-benchmark"
          . "cd snipycpp && uv run --no-progress --reinstall pytest --benchmark-enable")
         ("snipycpp::clean" . "rm -rf snipycpp/build")
         ("snipycpp::cmake" . "cmake --preset release")
         ("snipycpp::catch-test" . "cd build/release && ninja && ./tests/test_runner")
         ("snipy::pytest" . "cd ../snipy && pytest")))
     (compile-multi-config
      (t
       (#("snipycpp::pytest" 0 1
          (compile-multi--task "uv run --no-progress --reinstall pytest" consult--type
                               snipycpp))
        . "uv run --no-progress --reinstall pytest")
       (#("snipycpp::pytest-benchmark" 0 1
          (compile-multi--task
           "uv run --no-progress --reinstall pytest --benchmark-enable" consult--type
           snipycpp))
        . "uv run --no-progress --reinstall pytest --benchmark-enable")
       (#("snipycpp::clean" 0 1
          (compile-multi--task "rm -rf build" consult--type snipycpp))
        . "rm -rf build")
       (#("snipycpp::cmake" 0 1
          (compile-multi--task "cmake --preset release" consult--type snipycpp))
        . "cmake --preset release")
       (#("snipycpp::catch-test" 0 1
          (compile-multi--task "cd build/release && ninja && ./tests/test_runner"
                               consult--type snipycpp))
        . "cd build/release && ninja && ./tests/test_runner")
       (#("snipy::pytest" 0 1
          (compile-multi--task "cd ../snipy && pytest" consult--type snipy))
        . "cd ../snipy && pytest")))
     (projectile-project-compilation-dir . ".")
     (compile-multi-config
      (t
       (#("snipycpp::pytest" 0 1
          (compile-multi--task
           "cd ~/git/longshot/strategy/snipycpp && uv run --no-progress --reinstall pytest"
           consult--type snipycpp))
        . "cd ~/git/longshot/strategy/snipycpp && uv run --no-progress --reinstall pytest")
       (#("snipycpp::pytest-benchmark" 0 1
          (compile-multi--task
           "cd ~/git/longshot/strategy/snipycpp && uv run --no-progress --reinstall pytest --benchmark-enable"
           consult--type snipycpp))
        . "cd ~/git/longshot/strategy/snipycpp && uv run --no-progress --reinstall pytest --benchmark-enable")
       (#("snipycpp::clean" 0 1
          (compile-multi--task "rm -rf ~/git/longshot/strategy/snipycpp/build"
                               consult--type snipycpp))
        . "rm -rf ~/git/longshot/strategy/snipycpp/build")
       (#("snipycpp::cmake" 0 1
          (compile-multi--task
           "cd ~/git/longshot/strategy/snipycpp && cmake --preset release" consult--type
           snipycpp))
        . "cd ~/git/longshot/strategy/snipycpp && cmake --preset release")
       (#("snipycpp::catch-test" 0 1
          (compile-multi--task
           "cd ~/git/longshot/strategy/snipycpp/build/release && ninja && ./tests/test_runner"
           consult--type snipycpp))
        . "cd ~/git/longshot/strategy/snipycpp/build/release && ninja && ./tests/test_runner")
       (#("snipy::pytest" 0 1
          (compile-multi--task "cd ~/git/longshot/strategy/snipy && pytest"
                               consult--type snipy))
        . "cd ~/git/longshot/strategy/snipy && pytest")))
     (compile-multi-config
      (t
       ("snipycpp::pytest"
        . "cd ~/git/longshot/strategy/snipycpp && uv run --no-progress --reinstall pytest")
       ("snipycpp::pytest-benchmark"
        . "cd ~/git/longshot/strategy/snipycpp && uv run --no-progress --reinstall pytest --benchmark-enable")
       ("snipycpp::clean" . "rm -rf ~/git/longshot/strategy/snipycpp/build")
       ("snipycpp::cmake"
        . "cd ~/git/longshot/strategy/snipycpp/build/release && cmake --preset release")
       ("snipycpp::catch-test"
        . "cd ~/git/longshot/strategy/snipycpp/build/release && ninja && ./tests/test_runner")
       ("snipy::pytest" . "cd ~/git/longshot/strategy/snipy && pytest")))
     (compile-multi-config
      (t
       ("snipycpp::pytest"
        . "cd ~/git/longshot/strategy/snipycpp && uv run --no-progress --reinstall pytest")
       ("snipycpp::pytest-benchmark"
        . "cd ~/git/longshot/strategy/snipycpp && uv run --no-progress --reinstall pytest --benchmark-enable")
       ("snipycpp::clean" . "rm -rf ~/git/longshot/strategy/snipycpp/build")
       ("snipycpp::cmake"
        . "cd ~/git/longshot/strategy/snipycpp/build/release && cmake --preset release")
       ("snipycpp::catch-test"
        . "cd ~/git/longshot/strategy/snipycpp/build/release && ninja && ./tests/test_runner")
       ("snipy::pytest"
        . "cd ~/git/longshot/strategy/snipy && uv run --no-progress --reinstall pytest")))
     (compile-multi-config
      (t
       ("snipycpp::pytest"
        . "cd ~/git/longshot/strategy/snipycpp && uv run --no-progress --reinstall pytest")
       ("snipycpp::pytest-benchmark"
        . "cd ~/git/longshot/strategy/snipycpp && uv run --no-progress --reinstall pytest --benchmark-enable")
       ("snipycpp::clean" . "rm -rf ~/git/longshot/strategy/snipycpp/build")
       ("snipycpp::cmake"
        . "cd ~/git/longshot/strategy/snipycpp/build/release && cmake --preset release")
       ("snipycpp::catch-test"
        . "cd ~/git/longshot/strategy/snipycpp/build/release && ninja && ./tests/test_runner")))
     (compile-multi-config
      (t
       ("snipycpp::pytest"
        . "cd ~/git/longshot/strategy/snipycpp && uv run --no-progress --reinstall pytest")
       ("snipycpp::pytest-benchmark"
        . "cd ~/git/longshot/strategy/snipycpp/build/release && uv run --no-progress --reinstall pytest --benchmark-enable")
       ("snipycpp::clean" . "rm -rf ~/git/longshot/strategy/snipycpp/build")
       ("snipycpp::cmake"
        . "cd ~/git/longshot/strategy/snipycpp/build/release && cmake --preset release")
       ("snipycpp::catch-test"
        . "cd ~/git/longshot/strategy/snipycpp/build/release && ninja && ./tests/test_runner")))
     (compile-multi-config
      (t
       (#("snipycpp::pytest" 0 1
          (compile-multi--task
           "cd ~/git/longshot/strategy/snipycpp/build/release && uv run --no-progress --reinstall pytest"
           consult--type snipycpp))
        . "cd ~/git/longshot/strategy/snipycpp/build/release && uv run --no-progress --reinstall pytest")
       (#("snipycpp::pytest-benchmark" 0 1
          (compile-multi--task
           "cd ~/git/longshot/strategy/snipycpp/build/release && uv run --no-progress --reinstall pytest --benchmark-enable"
           consult--type snipycpp))
        . "cd ~/git/longshot/strategy/snipycpp/build/release && uv run --no-progress --reinstall pytest --benchmark-enable")
       (#("snipycpp::clean" 0 1
          (compile-multi--task "rm -rf ~/git/longshot/strategy/snipycpp/build"
                               consult--type snipycpp))
        . "rm -rf ~/git/longshot/strategy/snipycpp/build")
       (#("snipycpp::cmake" 0 1
          (compile-multi--task
           "cd ~/git/longshot/strategy/snipycpp/build/release && cmake --preset release"
           consult--type snipycpp))
        . "cd ~/git/longshot/strategy/snipycpp/build/release && cmake --preset release")
       (#("snipycpp::catch-test" 0 1
          (compile-multi--task
           "cd ~/git/longshot/strategy/snipycpp/build/release && ninja && ./tests/test_runner"
           consult--type snipycpp))
        . "cd ~/git/longshot/strategy/snipycpp/build/release && ninja && ./tests/test_runner")))
     (compile-multi-config
      (t
       ("pytest"
        . "cd ~/git/longshot/strategy/snipycpp/build/release && uv run --no-progress --reinstall pytest")
       ("pytest-benchmark"
        . "cd ~/git/longshot/strategy/snipycpp/build/release && uv run --no-progress --reinstall pytest --benchmark-enable")
       ("clean" . "rm -rf ~/git/longshot/strategy/snipycpp/build")
       ("cmake"
        . "cd ~/git/longshot/strategy/snipycpp/build/release && cmake --preset release")
       ("catch-test"
        . "cd ~/git/longshot/strategy/snipycpp/build/release && ninja && ./tests/test_runner")))
     (compile-multi-config
      (t ("pytest" . "uv run --no-progress --reinstall pytest")
         ("pytest-benchmark"
          . "cd ~/git/longshot/strategy/snipycpp/build/release && uv run --no-progress --reinstall pytest --benchmark-enable")
         ("clean" . "rm -rf ~/git/longshot/strategy/snipycpp/build")
         ("cmake"
          . "cd ~/git/longshot/strategy/snipycpp/build/release && cmake --preset release")
         ("catch-test"
          . "cd ~/git/longshot/strategy/snipycpp/build/release && ninja && ./tests/test_runner")))
     (compile-multi-config
      (t ("pytest" . "uv run --no-progress --reinstall pytest")
         ("pytest-benchmark"
          . "uv run --no-progress --reinstall pytest --benchmark-enable")
         ("clean" . "rm -rf ~/git/longshot/strategy/snipycpp/build")
         ("cmake"
          . "cd ~/git/longshot/strategy/snipycpp/build/release && cmake --preset release")
         ("catch-test"
          . "cd ~/git/longshot/strategy/snipycpp/build/release && ninja && ./tests/test_runner")))
     (compile-multi-config
      (t ("pytest" . "uv run --no-progress --reinstall pytest")
         ("pytest-benchmark"
          . "uv run --no-progress --reinstall pytest --benchmark-enable")
         ("clean" . "rm -rf ~/git/longshot/strategy/snipycpp/build/release/build")
         ("cmake"
          . "cd ~/git/longshot/strategy/snipycpp/build/release && cmake --preset release")
         ("catch-test"
          . "cd ~/git/longshot/strategy/snipycpp/build/release && ninja && ./tests/test_runner")))
     (compile-multi-config
      (t ("pytest" . "uv run --no-progress --reinstall pytest")
         ("pytest-benchmark"
          . "uv run --no-progress --reinstall pytest --benchmark-enable")
         ("clean" . "rm -rf build")
         ("cmake"
          . "cd ~/git/longshot/strategy/snipycpp/build/release && cmake --preset release")
         ("catch-test"
          . "cd ~/git/longshot/strategy/snipycpp/build/release && ninja && ./tests/test_runner")))
     (compile-multi-config
      (t ("pytest" . "uv run --no-progress --reinstall pytest")
         ("pytest-benchmark"
          . "uv run --no-progress --reinstall pytest --benchmark-enable")
         ("clean" . "rm -rf build") ("cmake" . "cmake --preset release")
         ("catch-test"
          . "cd ~/git/longshot/strategy/snipycpp/build/release && ninja && ./tests/test_runner")))
     (compile-multi-config
      (t ("pytest" . "uv run --no-progress --reinstall pytest")
         ("pytest-benchmark"
          . "uv run --no-progress --reinstall pytest --benchmark-enable")
         ("clean" . "rm -rf build") ("cmake" . "cmake --preset release")
         ("catch-test"
          . "cd ~/git/longshot/strategy/build/release && ninja && ./tests/test_runner")))
     (compile-multi-config
      (t
       (#("pytest" 0 1 (compile-multi--task "uv run --no-progress --reinstall pytest"))
        . "uv run --no-progress --reinstall pytest")
       (#("pytest-benchmark" 0 1
          (compile-multi--task
           "uv run --no-progress --reinstall pytest --benchmark-enable"))
        . "uv run --no-progress --reinstall pytest --benchmark-enable")
       (#("clean" 0 1 (compile-multi--task "rm -rf build")) . "rm -rf build")
       (#("cmake" 0 1 (compile-multi--task "cmake --preset release"))
        . "cmake --preset release")
       (#("catch-test" 0 1
          (compile-multi--task "cd build/release && ninja && ./tests/test_runner"))
        . "cd build/release && ninja && ./tests/test_runner")))
     (compile-multi-config
      (t ("snipycpp::pytest" . "cd snipycpp && uv run --no-progress --reinstall pytest")
         ("snipycpp::pytest-benchmark"
          . "cd snipycpp && uv run --no-progress --reinstall pytest --benchmark-enable")
         ("snipycpp::catch-test"
          . "cd snipycpp/build/release && ninja && ./tests/test_runner")
         ("snipycpp::catch-test"
          . "cd snipycpp/build/release && ninja && ./tests/test_runner")))
     (compile-multi-config
      (t
       (#("snipycpp::pytest" 0 1
          (compile-multi--task "cd snipycpp && uv run --no-progress --reinstall pytest"
                               consult--type snipycpp))
        . "cd snipycpp && uv run --no-progress --reinstall pytest")
       (#("snipycpp::pytest-benchmark" 0 1
          (compile-multi--task
           "cd snipycpp && uv run --no-progress --reinstall pytest --benchmark-enable"
           consult--type snipycpp))
        . "cd snipycpp && uv run --no-progress --reinstall pytest --benchmark-enable")
       (#("snipycpp::catch-test" 0 1
          (compile-multi--task
           "cd snipycpp/build/release && ninja && ./tests/test_runner" consult--type
           snipycpp))
        . "cd snipycpp/build/release && ninja && ./tests/test_runner")))
     (compile-multi-config
      (t
       (#("snipycpp::pytest" 0 1
          (compile-multi--task "cd snipycpp && uv run --no-progress --reinstall pytest"
                               consult--type snipycpp))
        . "cd snipycpp && uv run --no-progress --reinstall pytest")
       (#("snipycpp::pytest-benchmark" 0 1
          (compile-multi--task
           "cd snipycpp && uv run --no-progress --reinstall pytest --benchmark-enable"
           consult--type snipycpp))
        . "cd snipycpp && uv run --no-progress --reinstall pytest --benchmark-enable")
       (#("snipycpp::release::catch-test" 0 1
          (compile-multi--task "" consult--type snipycpp)))))
     (compile-multi-config
      (t
       (#("snipycpp::pytest" 0 1
          (compile-multi--task "cd snipycpp && uv run --no-progress --reinstall pytest"
                               consult--type snipycpp))
        . "cd snipycpp && uv run --no-progress --reinstall pytest")
       (#("snipycpp::pytest-benchmark" 0 1
          (compile-multi--task
           "cd snipycpp && uv run --no-progress --reinstall pytest --benchmark-enable"
           consult--type snipycpp))
        . "cd snipycpp && uv run --no-progress --reinstall pytest --benchmark-enable")
       (#("snipycpp::::release::catch-test" 0 1
          (compile-multi--task "" consult--type snipycpp)))))
     (compile-multi-config
      (t ("snipycpp::pytest" . "cd snipycpp && uv run --no-progress --reinstall pytest")
         ("snipycpp::pytest-benchmark"
          . "cd snipycpp && uv run --no-progress --reinstall pytest --benchmark-enable")))
     (compile-multi-config
      (t
       (#("snipycpp::pytest" 0 1
          (compile-multi--task "cd snipycpp && uv run --no-progress --reinstall pytest"
                               consult--type snipycpp))
        . "cd snipycpp && uv run --no-progress --reinstall pytest")
       (#("snipycpp::pytest-benchmark" 0 1
          (compile-multi--task
           "cd snipycpp && uv run --reinstall pytest --benchmark-enable" consult--type
           snipycpp))
        . "cd snipycpp && uv run --reinstall pytest --benchmark-enable")))
     (compile-multi-config
      (t
       (#("snipycpp::pytest" 0 1
          (compile-multi--task "cd snipycpp && uv run --reinstall pytest" consult--type
                               snipycpp))
        . "cd snipycpp && uv run --reinstall pytest")
       (#("snipycpp::pytest-benchmark" 0 1
          (compile-multi--task
           "cd snipycpp && uv run --reinstall pytest --benchmark-enable" consult--type
           snipycpp))
        . "cd snipycpp && uv run --reinstall pytest --benchmark-enable")))
     (compile-multi-config
      (t ("build::release" . "cmake --build build -j$(nproc)")
         ("build::debug" . "cmake --build build-debug -j$(nproc)")
         ("test::all" . "cd build && ctest --output-on-failure")
         ("test::verbose" . "cd build && ctest -V")
         ("clean" . "cmake --build build --target clean"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diredp-dir-name ((t (:foreground "#7474FFFFFFFF")))))
