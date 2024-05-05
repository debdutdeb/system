(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(ido-mode)
(tool-bar-mode -1)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; don't do anything basically
(defun my/ring-bell-function ())

(setq-default indent-tabs-mode t)
(setq tab-width 4)

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

(use-package vterm
  :straight t)

; (require 'treesit)

(load-library "clang-format.elc")

;; TODO refactor to a separate library like null-ls
(define-prefix-command 'lsp)
(global-set-key (kbd "C-c l") 'lsp)

;;
										; (global-set-key (kbd "C-c l f") 'clang-format-buffer) (global-set-key (kbd "C-c l r") 'clang-format-region)

(defun toggle-eglot ()
  (interactive)
  (if (eq (eglot-current-server) nil)
	  (eglot-ensure)
	(eglot-shutdown-all)))

(use-package evil
  :straight t
  :config
  (setq evil-shift-width 4)
  (evil-mode 1)
  (evil-set-initial-state 'vterm-mode 'emacs))

(with-eval-after-load 'evil
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>ll") 'toggle-eglot)
  (evil-define-key 'normal 'global (kbd "<leader>lf") 'eglot-format-buffer))

(use-package company
  :straight t
  :config
  (setq company-minimum-prefix-length 1
		company-idle-delay nil)
  :bind
  ("C-x C-o" . company-complete)
  ("C-c l f" . eglot-format-buffer)
  :hook
  (after-init . global-company-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
			   `(go-ts-mode . ("gopls"))))

(add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)

(set-face-attribute 'default nil :height 220)

(use-package treesit-auto
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
;  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(setq go-ts-config
	  (make-treesit-auto-recipe
	   :lang 'go
	   :ts-mode 'go-ts-mode
	   :url "https://github.com/tree-sitter/tree-sitter-go"
	   :revision "master"
	   :source-dir "src"))       

(add-to-list 'treesit-auto-recipe-list go-ts-config)

(set-frame-font "Inconsolata Nerd Font Mono" nil t)

(use-package edraw
  :straight (:type git :host github :repo "misohena/el-easydraw"))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("993aac313027a1d6e70d45b98e121492c1b00a0daa5a8629788ed7d523fe62c1"
     "330d5278ead8dd474f8e79d0cadae973aae3e56f86e6e6d1667d723992b34a59"
     "34af44a659b79c9f92db13ac7776b875a8d7e1773448a8301f97c18437a822b6"
     default))
 '(package-selected-packages
   '(straight vscode-dark-plus-theme treesit-auto popup leuven-theme evil
	      counsel company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
