(require 'magit)

(setq dotfiles-repo-checked nil)

(add-hook 'window-setup-hook
          (lambda ()
            (when (and (not dotfiles-repo-checked)
                       (y-or-n-p "Update the .dotfiles repo?"))
              (setq dotfiles-repo-checked 't)
              (magit-status-internal dotfiles-repo-path)
              (magit-fetch-from-pushremote '()))))

(defun check-for-dotfiles-changes ()
  (let* ((default-directory dotfiles-repo-path)
         (changed-items (magit-git-items "status" "--porcelain" "-z"))
         (upstream (magit-rev-parse "@{upstream}"))
         (unpushed (and upstream
                        (not (magit-rev-ancestor-p "HEAD" upstream)))))
    (message "Checking dotfiles")
    (or (and (null changed-items) (not unpushed))
        (y-or-n-p "There are some changes in .dotfiles repo, exit anyway?"))))

(add-to-list 'kill-emacs-query-functions
             #'check-for-dotfiles-changes)
