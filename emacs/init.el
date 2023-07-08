;; Initialize straight.el package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package if it's not already installed
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; don't do anything basically
(defun deb/ring-bell-function ())

(tool-bar-mode -1)
(setq inhibit-startup-message t
      visible-bell nil
      ring-bell-function 'deb/ring-bell-function
      display-line-numbers-type 'relative
      cursor-type 'box
      auto-save-default nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'prog-mode-hook 'display-line-numbers-mode)

;; custom el code
(add-to-list 'load-path "~/.config/emacs/custom")

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode))

(use-package evil
  :config
  (evil-mode)
  :init
  (setq evil-want-keybinding nil
	evil-want-C-u-scroll t
	evil-insert-state-cursor 'box
	evil-normal-state-cursor 'box
	evil-motion-state-cursor 'box
	evil-operator-state-cursor 'box
	evil-replace-state-cursor 'box
	evil-visual-state-cursor 'box
	;; maybe use undo-redo ? FIXME
	evil-undo-system 'undo-tree))

(use-package swiper
  :ensure t
  :config
  (ivy-mode)
  :init
  (setq ivy-display-style 'plain ; no need for fancy
	ivy-use-virtual-buffers t
	enable-recursive-minibuffers t
	ivy-wrap t))

(use-package counsel
  :ensure t
  :config (counsel-mode))

(global-set-key (kbd "C-h b") 'describe-bindings)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h i") 'info)
(global-set-key (kbd "C-h k") 'describe-key)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-s") 'swiper-isearch)

(require 'treesit)

(load-library "clang-format.elc")

;; TODO refactor to a separate library like null-ls
(define-prefix-command 'formatting)
(global-set-key (kbd "C-c l") 'formatting)

;;
(global-set-key (kbd "C-c l f") 'clang-format-buffer)
(global-set-key (kbd "C-c l r") 'clang-format-region)

