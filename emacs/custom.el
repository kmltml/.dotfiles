(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-level (quote non-interactive))
 '(ansi-color-names-vector
   (vector "#515151" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#66cccc" "#cccccc"))
 '(calc-highlight-selections-with-faces t)
 '(company-c-headers-path-system
   (quote
    ("/usr/include/" "/usr/local/include/" "/usr/include/c++/5/" "/usr/lib/llvm-3.8/include/")))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "8bb8a5b27776c39b3c7bf9da1e711ac794e4dc9d43e32a075d8aa72d6b5b3f59" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "82b67c7e21c3b12be7b569af7c84ec0fb2d62105629a173e2479e1053cff94bd" "9d91458c4ad7c74cf946bd97ad085c0f6a40c370ac0a1cbeb2e3879f15b40553" default)))
 '(ensime-typecheck-idle-interval 2)
 '(ensime-typecheck-interval 10)
 '(ensime-typecheck-when-idle nil)
 '(fci-rule-color "#515151")
 '(flx-ido-mode t)
 '(git-gutter:added-sign " ")
 '(git-gutter:deleted-sign " ")
 '(git-gutter:modified-sign " ")
 '(git-gutter:update-interval 10)
 '(graphviz-dot-indent-width 2)
 '(haskell-font-lock-symbols t)
 '(haskell-font-lock-symbols-alist
   (quote
    (("\\\\" . λ)
     ("." "∘" haskell-font-lock-dot-is-not-composition))))
 '(haskell-process-path-stack "c:/Users/Kamil/AppData/Roaming/Local/bin/stack.exe")
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-tags-on-save t)
 '(ido-ignore-files
   (quote
    ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\.agdai")))
 '(js-indent-level 2)
 '(magit-diff-refine-hunk (quote all))
 '(menu-bar-mode nil)
 '(neo-theme (quote classic))
 '(neo-vc-integration (quote (face char)))
 '(neo-window-width 40)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-link-frame-setup
   (quote
    ((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame))))
 '(package-selected-packages
   (quote
    (persistent-scratch srefactor avy cargo clipmon color-theme-sanityinc-tomorrow company company-auctex company-c-headers company-ghc company-ghci company-glsl company-math company-racer company-tern crux dashboard ediprolog edit-server ein ein-mumamo emmet-mode ensime erlang expand-region flx-ido ggtags git-gutter glsl-mode graphviz-dot-mode hl-line+ hydra idris-mode image+ indent-guide latex-pretty-symbols latex-preview-pane magit nasm-mode popup-imenu projectile projectile-ripgrep racer rainbow-mode request ripgrep shm slime smartparens ssh-agency tide typescript-mode use-package visual-regexp-steroids w3m web-mode whitespace-cleanup-mode zzz-to-char)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(prettify-symbols-unprettify-at-point (quote right-edge))
 '(projectile-mode t nil (projectile))
 '(revert-without-query (quote ("\\.pdf")))
 '(safe-local-variable-values (quote ((eval defun))))
 '(show-paren-mode t)
 '(speedbar-default-position (quote left))
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f2777a")
     (40 . "#f99157")
     (60 . "#ffcc66")
     (80 . "#99cc99")
     (100 . "#66cccc")
     (120 . "#6699cc")
     (140 . "#cc99cc")
     (160 . "#f2777a")
     (180 . "#f99157")
     (200 . "#ffcc66")
     (220 . "#99cc99")
     (240 . "#66cccc")
     (260 . "#6699cc")
     (280 . "#cc99cc")
     (300 . "#f2777a")
     (320 . "#f99157")
     (340 . "#ffcc66")
     (360 . "#99cc99"))))
 '(vc-annotate-very-old-color nil)
 '(verilog-auto-newline nil)
 '(web-mode-auto-close-style 2)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2d2d2d" :foreground "#cccccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "outline" :family "DejaVu Sans Mono"))))
 '(agda2-highlight-catchall-clause-face ((t (:underline "white"))))
 '(agda2-highlight-coinductive-constructor-face ((t (:foreground "#ffcc66"))))
 '(agda2-highlight-coverage-problem-face ((t (:underline "#ffcc66"))))
 '(agda2-highlight-datatype-face ((t (:foreground "#6699cc"))))
 '(agda2-highlight-error-face ((t (:foreground "#f2777a" :underline t))))
 '(agda2-highlight-field-face ((t (:foreground "#cc99cc"))))
 '(agda2-highlight-function-face ((t (:foreground "#6699cc"))))
 '(agda2-highlight-inductive-constructor-face ((t (:foreground "#99cc99"))))
 '(agda2-highlight-keyword-face ((t (:foreground "#f99157"))))
 '(agda2-highlight-macro-face ((t (:foreground "#66cccc"))))
 '(agda2-highlight-module-face ((t (:foreground "#cc99cc"))))
 '(agda2-highlight-number-face ((t (:foreground "#cc99cc"))))
 '(agda2-highlight-positivity-problem-face ((t (:underline "#f99157"))))
 '(agda2-highlight-postulate-face ((t (:foreground "#6699cc"))))
 '(agda2-highlight-primitive-face ((t (:foreground "#6699cc"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "#6699cc"))))
 '(agda2-highlight-reachability-problem-face ((t (:underline "#cccccc"))))
 '(agda2-highlight-record-face ((t (:foreground "#6699cc"))))
 '(agda2-highlight-string-face ((t (:foreground "#f2777a"))))
 '(agda2-highlight-symbol-face ((t (:foreground "#f99157"))))
 '(agda2-highlight-termination-problem-face ((t (:underline "#f99157"))))
 '(agda2-highlight-typechecks-face ((t (:background "#6699cc" :foreground "#2d2d2d"))))
 '(agda2-highlight-unsolved-constraint-face ((t (:underline "#ffcc66"))))
 '(agda2-highlight-unsolved-meta-face ((t (:underline "#ffcc66"))))
 '(ensime-implicit-highlight ((t (:underline "dim gray"))))
 '(idris-active-term-face ((t (:underline "d6d6d6"))))
 '(idris-prover-processed-face ((t (:underline "#99cc99"))))
 '(idris-prover-processing-face ((t (:underline "#ffcc66"))))
 '(idris-semantic-bound-face ((t (:foreground "#cc99cc"))))
 '(idris-semantic-data-face ((t (:foreground "#f2777a"))))
 '(idris-semantic-function-face ((t (:foreground "#6699cc")))))
