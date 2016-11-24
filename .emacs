(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

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

(global-set-key (kbd "<escape>") 'keyboard-quit)
(global-set-key (kbd "S-<delete>") 'kill-whole-line)
(global-set-key (kbd "<f5>") 'projectile-compile-project)
(define-key c++-mode-map (kbd "C-;") (lambda () (interactive) (end-of-line) (insert ";")))
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

;; HTML + JS editting stuff
(add-to-list 'company-backends 'company-tern)

(add-hook 'html-mode-hook 'emmet-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-c-headers-path-system
   (quote
    ("/usr/include/" "/usr/local/include/" "/usr/include/c++/5/" "/usr/lib/llvm-3.8/include/")))
 '(custom-enabled-themes (quote (tango-dark)))
 '(package-selected-packages (quote (company-c-headers company sr-speedbar ggtags)))
 '(speedbar-default-position (quote left))
 ;; git-gutter
 '(git-gutter:update-interval 10)
 '(git-gutter:modified-sign " ")
 '(git-gutter:deleted-sign " ")
 '(git-gutter:added-sign " "))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
