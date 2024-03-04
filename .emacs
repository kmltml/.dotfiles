(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(load "~/.emacs.local" 'missing-ok)

(setq custom-file (concat dotfiles-repo-path "emacs/custom.el"))
(load custom-file)

(setenv "SSH_ASKPASS" "git-gui--askpass")

(setq
 backup-by-copying t
 backup-directory-alist '(("." . "~/.saves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

(toggle-scroll-bar -1)
(scroll-bar-mode -1)

(set-fontset-font "fontset-default" '(#x2113 . #x2113) "Consolas")

(setq-default indent-tabs-mode nil)

(setq-default show-trailing-whitespace t)

(setq show-paren-delay 0)

(setq quail-japanese-use-double-n t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "<insert>") 'iso-transl-ctl-x-8-map)
(define-key key-translation-map (kbd "<menu>") (kbd "<apps>"))

(defvar greek-insert-map (make-sparse-keymap))

(load-file (concat dotfiles-repo-path "emacs/greek.el"))
(dolist (p greek-input-pairs)
  (define-key greek-insert-map (kbd (car p)) (cdr p)))
(define-key 'iso-transl-ctl-x-8-map (kbd "g") greek-insert-map)

(defun add-prettify-rules (rules)
  (setq prettify-symbols-alist (append rules prettify-symbols-alist)))

(add-hook 'c-mode-hook
          (lambda ()
            (add-prettify-rules '(("->" . 8594)
                                  ("^" . 8853)
                                  (">=" . 8805)
                                  ("<=" . 8804)
                                  ("NULL" . 8709)
                                  ("!=" . 8800)
                                  ("!" . 172)))
            (prettify-symbols-mode)
            (semantic-mode)))

(with-eval-after-load 'cc-mode
  (define-key c-mode-map (kbd "<apps> m") 'man))

(defun term-dwim ()
  (interactive)
  (if (equalp major-mode 'term-mode)
      (delete-window)
    (let ((window (split-window nil -10)))
      (select-window window)
      (if (equalp (get-buffer "*terminal*") nil)
          (call-interactively 'term)
        (display-buffer "*terminal*" display-buffer--same-window-action)))))

(global-set-key (kbd "C-c t") 'term-dwim)

(setq-default cursor-type `(bar . 2))

(global-auto-revert-mode)

(delete-selection-mode 1)

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun upcase-first (s)
  (concat
   (upcase (substring s 0 1))
   (substring s 1)))

(defun downcase-first (s)
  (concat
   (downcase (substring s 0 1))
   (substring s 1)))

(defun custom-move-to-beginning-of-line ()
  (interactive "^")
  (let ((point-before-move (point)))
    (back-to-indentation)
    (when (= point-before-move (point))
      (move-beginning-of-line nil))))

(global-set-key (kbd "C-a") 'custom-move-to-beginning-of-line)
(global-set-key (kbd "<home>") 'custom-move-to-beginning-of-line)


(defun scala-split-or-merge-package ()
  (interactive)
  (let ((package-line-regexp "^\\s-*package\\s-+\\sw+\\(\\s-*\\.\\s*\\sw+\\)*")
        (line (thing-at-point 'line t)))
    (when (string-match package-line-regexp line)
      (let* ((pos (current-column))
             (stop-pos (cl-position ?. line :end pos :from-end t)))
        (if stop-pos
            (progn
              (delete-region (+ stop-pos (line-beginning-position)) (line-end-position))
              (insert ?\n "package " (substring line (+ stop-pos 1) -1)))
          (let ((before (point)))
            (previous-line)
            (if (string-match package-line-regexp
                              (thing-at-point 'line t))
                (let ((path-start (progn (string-match "package\\s-+\\(\\sw\\)" line)
                                         (match-beginning 1))))
                  (next-line)
                  (delete-region (line-beginning-position)
                                 (line-end-position))
                  (previous-line)
                  (end-of-line)
                  (insert ?. (substring line path-start -1))
                  (delete-region (line-end-position)
                                 (+ 1 (line-end-position))))
              (goto-char before))))))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

(defun my-origami-import-parser (create-fun)
  (lexical-let ((create create-fun))
    (lambda (content)
      (with-temp-buffer
        (insert content)
        (beginning-of-buffer)
        (search-forward-regexp (rx line-start "import"))
        (beginning-of-line)
        (let ((start (point)))
          (while (looking-at (rx (or "import" (seq (* space) line-end))))
            (next-line))
          (while (not (looking-at "import"))
            (previous-line))
          (next-line)
          (list (funcall create start (- (point) 1) 7 nil)))))))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(setq mouse-wheel-progressive-speed nil)

(setq read-file-name-completion-ignore-case 't)

;; TEMP fixes for magit
(defalias 'compat-alist-get 'alist-get)

(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package dired
  :straight nil
  :config
  (setq dired-listing-switches "-alh"))

(use-package delight)

(use-package spinner)

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-eighties))

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (global-set-key (kbd "C-x C-r") 'recentf-open-files))

(use-package company
  :delight
  :bind (("M-/" . company-complete)
         :map company-active-map
              ("<escape>" . company-abort))
  :config
  (global-company-mode)
  (setq lsp-completion-provider :capf))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package scala-mode
  :mode "\\.sc"
  :config
  (add-hook 'scala-mode-hook
            (lambda ()
              (add-prettify-rules '(("<=" . ?≤)
                                    (">=" . ?≥)
                                    ("!=" . 8800)
                                    ("<+>" . ?⊕)))
              (prettify-symbols-mode 1)
              (setq-local parens-require-spaces nil)))
  :bind (:map scala-mode-map
              ("<apps> ." . scala-split-or-merge-package)))

(use-package lsp-mode
  :hook
  ((scala-mode
    ; lsp is broken in current godot
    ;; gdscript-mode
    ) . lsp)
  :bind
  (("C-c l i" . lsp-ui-peek-find-implementation)
   ("C-c l s" . lsp-signature-activate)
   ("C-c l r" . lsp-ui-peek-find-references)
   ("C-c l n" . lsp-rename)
   ("C-c l e" . lsp-treemacs-errors-list)
   ("C-c l l" . lsp)
   ("C-c l a" . lsp-execute-code-action)
   ("C-c l f" . lsp-format-buffer))
  :config
  (add-hook 'lsp-mode-hook
            (lambda ()
              ;; (setq-local er/try-expand-list
              ;;             (append er/try-expand-list
              ;;                     '(lsp-extend-selection)))
              )))

(use-package lsp-ui
  :bind
  (("C-c l d" . lsp-ui-doc-glance))
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil))

(use-package lsp-java
  :config
  (setq lsp-java-format-enabled nil
        lsp-java-format-on-type-enabled nil))

(use-package lsp-metals
  :bind
  (("C-c l m i" . lsp-metals-toggle-show-implicit-arguments)
   ("C-c l m s" . lsp-metals-goto-super-method)))

(use-package git-gutter
  :delight
  :config
  (global-git-gutter-mode t)
  (set-face-background 'git-gutter:modified "#ffcc66")
  (set-face-background 'git-gutter:added    "#99cc99")
  (set-face-background 'git-gutter:deleted  "#f2777a"))

(use-package indent-guide
  :delight
  :config
  (indent-guide-global-mode)
  (setq-default indent-guide-recursive t)
  (setq-default indent-guide-char "│"))

(use-package expand-region
  :bind (("C-c w" . er/expand-region)))

(use-package magit
  :init
  (load-file (concat dotfiles-repo-path "emacs/dotfiles-update.el"))
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-file-dispatch)))

(use-package hydra
  :config
  (defhydra hydra-resize (global-map "C-c R")
    "resize window"
    ("<up>" enlarge-window)
    ("<down>" shrink-window)
    ("<left>" (shrink-window 1 t))
    ("<right>" (enlarge-window 1 t)))
  (defhydra hydra-edit (global-map "C-c")
    ("k" crux-kill-whole-line)
    ("d" crux-duplicate-current-line-or-region)
    ("<up>" move-line-up)
    ("<down>" move-line-down)
    ("RET" nil)))

(use-package projectile
  :delight
  :config
  (projectile-mode +1)
  (add-to-list 'projectile-globally-ignored-file-suffixes ".class")
  (projectile-update-project-type
   'bloop
   :test-suffix "Test")
  (setq projectile-create-missing-test-files t)
  :bind (:map projectile-mode-map
              ("C-c p" . 'projectile-command-map)))

(use-package ripgrep)

(use-package avy
  :bind (("C-c a" . avy-goto-char-2)
         ("C-c A" . avy-goto-char)))

(use-package helm
  :config
  (helm-mode 1)
  (add-to-list 'helm-completing-read-handlers-alist
               '(dired-do-rename . nil))
  (add-to-list 'helm-completing-read-handlers-alist
               '(dired-do-copy . nil))
  (setq helm-buffer-max-length 60)
  :bind (("C-x b" . helm-mini)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)))

(use-package helm-projectile
  :bind
  (:map projectile-command-map
        ("p" . helm-projectile-switch-project)))

(use-package helm-rg
  :bind (:map projectile-command-map
              ("g" . helm-projectile-rg)))

(use-package yasnippet
  :demand
  :delight 'yas-minor-mode
  :config
  (add-to-list 'yas-snippet-dirs (concat dotfiles-repo-path "emacs/snippets"))
  (yas-global-mode 1)
  :bind (("C-c y" . yas-insert-snippet)))

;; HTML + JS editting stuff
(use-package company-tern
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package emmet-mode
  :config
  (add-hook 'html-mode-hook 'emmet-mode)
  :bind (:map emmet-mode-keymap
              ("C-<return>" . nil)))

(use-package web-mode
  :config
  (add-hook 'web-mode-hook 'emmet-mode))

;; Org mode stuff
(use-package org
  :config
  (setq org-support-shift-select t
        org-format-latex-options (plist-put org-format-latex-options
                                            :scale 1.5))

  :bind (("C-c L" . org-store-link)
         ("C-c G" . org-agenda)
         :map org-mode-map
              ("<apps> w" . org-retrieve-link-url)))

;; (use-package ob
;;   :config
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((dot . t)
;;      (emacs-lisp . t)
;;      (plantuml . t)
;;      (gnuplot . t))))

(defun org-retrieve-link-url ()
  (interactive)
  (let* ((link (assoc :link (org-context)))
         (text (buffer-substring-no-properties (or (nth 1 link) (point-min))
                                               (or (nth 2 link) (point-max))))
         (end (string-match-p (regexp-quote "]") text)))
    (kill-new (if end
                  (substring text 2 end)
                text))))

(defun org-dblock-write:dir-listing (params)
  (let* ((dir (file-name-directory buffer-file-name))
         (files (seq-filter
                 (lambda (f) (not (equal f buffer-file-name)))
                 (directory-files dir :FULL "^[a-zA-Z].+?\\.org")))
         (subdirs (seq-filter
                   (lambda (d) (and (not (s-suffix? "/." d))
                                    (not (s-suffix? "/.." d))
                                    (file-directory-p d)
                                    (file-exists-p (concat d "/index.org"))))
                   (directory-files dir :FULL)))
         (links (seq-concatenate 'list
                                 (seq-map (lambda (f) (cons f (file-name-base f)))
                                          files)
                                 (seq-map (lambda (d) (cons (concat d "/index.org") (file-name-base d)))
                                          subdirs))))
    (when (file-exists-p (concat dir "/../index.org"))
      (insert (format "- [[%s][..]]\n" (concat dir "/../index.org"))))
    (dolist (l links)
      (insert (format "- [[%s][%s]]\n" (car l) (cdr l))))))

;; LaTeX stuff
(use-package latex
  :straight nil
  :config
  (when (string-equal "windows-nt" system-type)
    (setq doc-view-ghostscript-program "gswin64c"))
  :hook (LaTeX-mode . prettify-symbols-mode)
  :bind (:map LaTeX-mode-map
              ("<apps> o" . latex-insert-block)))


;; Haskell
(use-package haskell-mode
  :config
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-indentation-mode)))

(defun custom-popup-imenu (arg)
  (interactive "P")
  (when arg
    (setq imenu--index-alist nil))
  (popup-imenu))

(use-package popup-imenu
  :bind (("C-c i" . custom-popup-imenu)))

(use-package smartparens
  :demand
  :delight
  :config
  (smartparens-global-mode 1)
  (setq sp-ignore-modes-list (remove 'minibuffer-inactive-mode sp-ignore-modes-list))
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'latex-mode "\\[" "\\]")
  (sp-local-pair 'org-mode "\\[" "\\]")
  (sp-local-pair 'agda2-mode "{!" "!}")
  (sp-local-pair 'agda2-mode "⟪" "⟫")
  (sp-local-pair 'agda2-mode "⟨" "⟩")
  (sp-local-pair 'agda2-mode "⟦" "⟧")
  (sp-local-pair 'scala-mode "/*" "*/")
  :bind (("C-c s u" . sp-splice-sexp)
         ("C-c s r" . sp-rewrap-sexp)))

(use-package zzz-to-char
  :bind (("M-z" . #'zzz-to-char-up-to-char)))

(use-package visual-regexp
  :bind (("C-c r" . vr/query-replace)))

(use-package crux
  :defer 10
  :bind (("C-<return>" . crux-smart-open-line-above)
         ("S-<return>" . crux-smart-open-line)
         ("C-c e" . crux-eval-and-replace)))

(use-package clipmon
  :init
  (add-to-list 'after-init-hook 'clipmon-mode-start))

(use-package whitespace-cleanup-mode
  :delight
  :config
  (global-whitespace-cleanup-mode))

(use-package nasm-mode
  :mode ("\\.asm" . nasm-mode))

(use-package flycheck
  :hook ((c-mode . flycheck-mode)))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package erlang
  :config
  (load-file (concat dotfiles-repo-path "emacs/erlang.el"))
  :bind
  (:map erlang-mode-map
        ("<apps> e" . erlang-export-at-point))
  :hook (((erlang-mode erlang-shell-mode) .
          (lambda ()
            (add-prettify-rules '(("->" . 8594)
                                  ("<-" . 8592)
                                  (">=" . 8805)
                                  ("=<" . 8804)
                                  ("=>" . 8658)
                                  ("<=" . 8656)
                                  ("||" . 8214)))
            (prettify-symbols-mode)))
         (erlang-mode . flycheck-mode)))

(use-package windmove
  :config
  (windmove-default-keybindings 'meta))

(use-package which-key
  :delight
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(use-package spaceline
  :config
  (require 'spaceline-segments)
  (spaceline-define-segment spinner
    (spinner-print spinner-current))
  (spaceline-compile
    '((buffer-modified
       :priority 10
       :face highlight-face)
      (buffer-id :priority 9)
      (major-mode :priority 5)
      ((flycheck-error flycheck-warning flycheck-info)
       :when active
       :priority 1)
      (minor-modes :priority 4))
    '(spinner
      (projectile-root :priority 2)
      (line-column
       :face highlight-face
       :priority 10)))
  (setq spaceline-minor-modes-separator " ")
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(use-package origami
  :init
  (global-origami-mode)
  ;; (add-hook )
  :bind
  ("C-c o" . origami-toggle-node))

(use-package javascript-mode
  :mode "\\.mjs")

(use-package typescript-mode
  :config
  (put 'typescript-insert-and-indent 'delete-selection nil))

(use-package rust-mode
  :config
  (setq rust-indent-offset 2))

(use-package lua-mode)

(use-package kotlin-mode)

(use-package gdscript-mode
  :bind
  (:map gdscript-mode-map
        ("C-c r" . nil))
  :config
  (setq gdscript-use-tab-indents nil
        gdscript-indent-offset 2))

(use-package yaml-mode)

(use-package smerge-mode
  :bind (:map smerge-mode-map
              ("C-c m" . hydra-smerge/body))
  :config
  (defhydra hydra-smerge (:color pink
                          :hint nil
                          :pre (smerge-mode 1)
                          :post (smerge-auto-leave))
      "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("R" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("k" smerge-kill-current)
      ("q" nil "cancel" :color blue)))

(use-package string-inflection)

(load "~/.emacs.d/local-init.el" 'missing-ok)
