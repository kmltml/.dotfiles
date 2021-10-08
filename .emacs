(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(require 'use-package)
(require 'subr-x)

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

(setq show-paren-delay 0)

(setq quail-japanese-use-double-n t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "S-<delete>") 'kill-whole-line)
(global-set-key (kbd "<f5>") 'projectile-compile-project)
(global-set-key (kbd "C-;") (lambda () (interactive) (end-of-line) (insert ";")))
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

(defun scala-prettify-compose-predicate (start end s)
  (and (if (string-equal s "*")
           (string-match (rx (or whitespace "\n") "*" (or whitespace "\n"))
                         (buffer-substring-no-properties (- start 1) (+ end 1)))
         t)
       (prettify-symbols-default-compose-p start end s)))

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

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(setq mouse-wheel-progressive-speed nil)

(setq read-file-name-completion-ignore-case 't)

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (global-set-key (kbd "C-x C-r") 'recentf-open-files))

(use-package company
  :bind (("C-c SPC" . company-complete)
         ("M-/" . company-complete)
         :map company-active-map
              ("<escape>" . company-abort))
  :config
  (global-company-mode))

(use-package scala-mode
  :config
  (add-hook 'scala-mode-hook
            (lambda ()
              (push '("*" . #x22c5) prettify-symbols-alist)
              (setq prettify-symbols-compose-predicate
                    'scala-prettify-compose-predicate)
              (prettify-symbols-mode 1)))
  :bind (:map scala-mode-map
              ("<apps> ." . scala-split-or-merge-package)))

(use-package lsp-mode
  :demand
  :hook (scala-mode . lsp))

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
  :bind (("C-x g" . magit-status)))

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

(use-package paredit
  :config
  (unbind-key "C-<right>" paredit-mode-map)
  (unbind-key "C-<left>" paredit-mode-map))

(use-package projectile
  :delight
  :config
  (add-to-list 'projectile-globally-ignored-file-suffixes ".class")
  :bind (:map projectile-mode-map
              ("C-c p" . 'projectile-command-map)))

(use-package avy
  :bind (("C-c a" . avy-goto-char-2)
         ("C-c A" . avy-goto-char)))

(use-package ido
  :config
  (ido-mode t))

(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs (concat dotfiles-repo-path "emacs/snippets"))
  (yas-global-mode 1)
  :bind (("C-<tab>" . yas-expand)
         ("C-c y" . yas-expand)))

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

  :bind (:map org-mode-map
              ("<apps> w" . org-retrieve-link-url)))

(use-package ob
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (emacs-lisp . t)
     (plantuml . t)
     (gnuplot . t))))

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
  :hook ((emacs-lisp-mode . (lambda ()
                              (sp-pair "'" nil :actions :rem))))
  :config
  (smartparens-global-mode nil)
  (setq sp-ignore-modes-list (remove 'minibuffer-inactive-mode sp-ignore-modes-list))
  (sp-local-pair 'latex-mode "\\[" "\\]")
  (sp-local-pair 'org-mode "\\[" "\\]")
  (sp-local-pair 'agda2-mode "{!" "!}")
  (sp-local-pair 'agda2-mode "⟪" "⟫")
  (sp-local-pair 'agda2-mode "⟨" "⟩")
  (sp-local-pair 'agda2-mode "⟦" "⟧")
  :bind (("C-c s u" . sp-splice-sexp)
         ("C-c s r" . sp-rewrap-sexp)))

(use-package zzz-to-char
  :bind (("M-z" . #'zzz-up-to-char)))

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
  :demand
  :delight
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(use-package spaceline
  :demand
  :config
  (require 'spaceline-segments)
  (spaceline-define-segment custom-version-control
    "Version control information."
    (when vc-mode
      (powerline-raw
       (s-trim (concat vc-mode
                       (when (buffer-file-name)
                         (pcase (vc-state (buffer-file-name))
                           (`up-to-date " ")
                           (`edited " *")
                           (`added " +")
                           (`unregistered " ?")
                           (`removed " -")
                           (`needs-merge " /")
                           (`needs-update " ↓")
                           (`ignored " .")
                           (_ " _"))))))))

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
    '((projectile-root :priority 2)
      (custom-version-control :priority 3)
      (line-column
       :face highlight-face
       :priority 10)))
  (setq spaceline-minor-modes-separator " ")
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(use-package typescript-mode
  :config
  (put 'typescript-insert-and-indent 'delete-selection nil))

(use-package rust-mode
  :config
  (setq rust-indent-offset 2))
