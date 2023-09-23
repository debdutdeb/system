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
(defun my/ring-bell-function ())

(tool-bar-mode -1)

; (ido-mode)
(setq inhibit-startup-message t
      visible-bell nil
      ring-bell-function 'my/ring-bell-function
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
(global-set-key (kbd "C-x C-f") 'counsel-fzf) ; this is a remap of counsel-find-file

(require 'treesit)

(load-library "clang-format.elc")

;; TODO refactor to a separate library like null-ls
(define-prefix-command 'formatting)
(global-set-key (kbd "C-c l") 'formatting)

;;
(global-set-key (kbd "C-c l f") 'clang-format-buffer)
(global-set-key (kbd "C-c l r") 'clang-format-region)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       `(go-ts-mode . ("gopls")))
  (add-to-list 'eglot-server-programs
	       `(cc-mode . ("/opt/homebrew/opt/llvm/bin/clangd")))
  (add-to-list 'eglot-server-programs
	       `(c++-mode . ("/opt/homebrew/opt/llvm/bin/clangd")))
  (add-to-list 'eglot-server-programs
	       `(zig-mode . ("zls")))
  (add-to-list 'eglot-server-programs
		`(c-mode . ("/opt/homebrew/opt/llvm/bin/clangd"))))

(add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)

(straight-use-package '(xkcd-303-mode :type git :host github :repo "elizagamedev/xkcd-303-mode.el"))

(use-package zig-mode)

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

;; add treesit-install-language-grammar

;; FIXME: use treesit-language-source-alist
; (treesit--install-language-grammar-1 (concat user-emacs-directory "tree-sitter") 'comment "https://github.com/stsewd/tree-sitter-comment")

(set-frame-font "UbuntuMono Nerd Font 18" nil t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wheatgrass)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
