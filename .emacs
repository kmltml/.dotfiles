(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(require 'use-package)

(load "~/.emacs.local" 'missing-ok)

(setq custom-file (s-concat dotfiles-repo-path "emacs/custom.el"))
(load custom-file)

(setenv "SSH_ASKPASS" "git-gui--askpass")

(add-to-list 'default-frame-alist '(font . "Fira Code"))

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

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

(setq-default indent-tabs-mode nil)

(require 'ggtags)
(add-hook 'c-mode-common-hook
	  (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'cc-mode)

(setq company-backends (delete 'company-semantic company-backends))
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-backends 'company-irony)
(define-key c-mode-map (kbd "C-SPC") 'company-complete)
(define-key c++-mode-map (kbd "C-SPC") 'company-complete)

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(global-semantic-idle-summary-mode 1)

(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

(semantic-mode 1)

(show-paren-mode 1)
(setq show-paren-delay 0)

(electric-pair-mode)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key company-active-map (kbd "<escape>") 'company-abort)
(global-set-key (kbd "S-<delete>") 'kill-whole-line)
(global-set-key (kbd "<f5>") 'projectile-compile-project)
(global-set-key (kbd "C-;") (lambda () (interactive) (end-of-line) (insert ";")))
(define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file)

(setq-default cursor-type `(bar . 2))

(global-auto-revert-mode)

(delete-selection-mode 1)

(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

(tool-bar-mode -1)
(menu-bar-mode -1)

(defun stubgen () (interactive)
  (shell-command (concat "stubgen -lNn " (buffer-file-name))))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(global-set-key (kbd "M-<up>") 'move-line-up)

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "M-<down>") 'move-line-down)

(defun custom-move-to-beginning-of-line ()
  (interactive "^")
  (let ((point-before-move (point)))
    (back-to-indentation)
    (when (= point-before-move (point))
      (move-beginning-of-line nil))))

(global-set-key (kbd "C-a") 'custom-move-to-beginning-of-line)
(global-set-key (kbd "<home>") 'custom-move-to-beginning-of-line)

;; Scala thingies

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

(with-eval-after-load 'scala-mode
  (bind-key (kbd "C-c .") 'scala-split-or-merge-package scala-mode-map))

(require 'neotree)
(global-set-key (kbd "<f8>") 'neotree)

(define-key neotree-mode-map (kbd "<escape>")
  (lambda ()
    (interactive)
    (select-window (previous-window))))

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(require 'git-gutter)
(global-git-gutter-mode t)

(set-face-background 'git-gutter:modified "dark orange") ;; background color
(set-face-background 'git-gutter:added    "dark green")
(set-face-background 'git-gutter:deleted  "dark red")

(require 'indent-guide)

(indent-guide-global-mode)

(setq-default indent-guide-recursive t)

(setq-default indent-guide-char "│")

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
    
(setq mouse-wheel-progressive-speed nil)

(setq read-file-name-completion-ignore-case 't)

(global-set-key (kbd "C-c SPC") 'company-complete)

(with-eval-after-load 'expand-region
  (global-set-key (kbd "C-c w") 'er/expand-region))


(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package hydra
  :config
  (defhydra hydra-resize (global-map "C-c C-r")
    "resize window"
    (">" enlarge-window)
    ("<" shrink-window)))

(use-package paredit
  :config
  (unbind-key "C-<right>" paredit-mode-map)
  (unbind-key "C-<left>" paredit-mode-map))

(use-package projectile
  :config
  (add-to-list 'projectile-globally-ignored-file-suffixes ".class"))

(setq ensime-startup-notification nil)

(use-package avy
  :bind (("C-'" . avy-goto-char-2)
	 ("C-\"" . avy-goto-char)))

(use-package ido
  :config
  (ido-mode t))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  :bind (("C-<tab>" . yas-expand)))

;; HTML + JS editting stuff
(add-to-list 'company-backends 'company-tern)

(add-hook 'html-mode-hook 'emmet-mode)

;; math-input-mode
(use-package xah-math-input-mode
  :bind (:map xah-math-input-keymap
	 ("S-SPC" . nil)
	 ("C-c e" . xah-math-input-change-to-symbol)))

;; Rust stuff
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(add-hook 'cargo-process-mode-hook (lambda () (setq truncate-lines nil)))

(require 'rust-mode )

(define-key rust-mode-map (kbd "C-c s") 'find-rust-symbols)

(defun find-rust-symbols ()
  (interactive)
  (occur "\\<fn\\>\\|\\<struct\\>\\|\\<enum\\>\\|\\<impl\\>")
  (switch-to-buffer-other-window "*Occur*"))

;; Org mode stuff
(setq org-support-shift-select t)
