(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-level 'non-interactive)
 '(ansi-color-names-vector
   (vector "#515151" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#66cccc" "#cccccc"))
 '(calc-highlight-selections-with-faces t)
 '(company-c-headers-path-system
   '("/usr/include/" "/usr/local/include/" "/usr/include/c++/5/" "/usr/lib/llvm-3.8/include/"))
 '(custom-safe-themes
   '("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "58fb295e041032fd7a61074ca134259dfdef557ca67d37c4240dbfbb11b8fcc7" default))
 '(fci-rule-color "#515151")
 '(flx-ido-mode t)
 '(git-gutter:added-sign " ")
 '(git-gutter:deleted-sign " ")
 '(git-gutter:modified-sign " ")
 '(git-gutter:update-interval 10)
 '(graphviz-dot-indent-width 2)
 '(haskell-font-lock-symbols t)
 '(haskell-font-lock-symbols-alist
   '(("\\\\" . λ)
     ("." "∘" haskell-font-lock-dot-is-not-composition)))
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-tags-on-save t)
 '(ido-ignore-files
   '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\.agdai"))
 '(js-indent-level 2)
 '(magit-diff-refine-hunk 'all)
 '(menu-bar-mode nil)
 '(neo-theme 'classic)
 '(neo-vc-integration '(face char))
 '(neo-window-width 40)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame)))
 '(package-selected-packages
   '(lsp-python-ms gnu-elpa-keyring-update gnuplot psci psc-ide purescript-mode scala-mode pinentry lsp-ui company-lsp lsp-scala lua-mode alchemist elixir-mode delight spaceline which-key ruby-end persistent-scratch srefactor avy cargo clipmon color-theme-sanityinc-tomorrow company company-auctex company-c-headers company-ghc company-ghci company-glsl company-math company-racer company-tern crux ediprolog edit-server ein ein-mumamo emmet-mode erlang expand-region flx-ido ggtags git-gutter glsl-mode graphviz-dot-mode hl-line+ hydra idris-mode image+ indent-guide latex-pretty-symbols latex-preview-pane magit nasm-mode popup-imenu projectile projectile-ripgrep racer rainbow-mode request ripgrep shm slime smartparens ssh-agency tide typescript-mode use-package visual-regexp-steroids w3m web-mode whitespace-cleanup-mode zzz-to-char))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(prettify-symbols-unprettify-at-point 'right-edge)
 '(projectile-mode t)
 '(revert-without-query '("\\.pdf"))
 '(safe-local-variable-values '((eval defun)))
 '(show-paren-mode t)
 '(speedbar-default-position 'left)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#f2777a")
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
     (360 . "#99cc99")))
 '(vc-annotate-very-old-color nil)
 '(verilog-auto-newline nil)
 '(web-mode-auto-close-style 2)
 '(web-mode-code-indent-offset 2)
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
