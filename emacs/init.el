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
(require 'use-package)

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

(load-library "clang-format")


;;
										; (global-set-key (kbd "C-c l f") 'clang-format-buffer) (global-set-key (kbd "C-c l r") 'clang-format-region)

(add-to-list 'exec-path (expand-file-name "~/go/bin"))
(setenv "PATH"
        (concat (expand-file-name "~/go/bin")
                ":"
                (getenv "PATH")))
(add-to-list 'exec-path (expand-file-name "~/.nix-profile/bin"))
(setenv "PATH"
        (concat (expand-file-name "~/.nix-profile/bin")
                ":"
                (getenv "PATH")))

(setq evil-want-keybinding nil)

(use-package evil
  :straight t
  :config
  (setq evil-shift-width 4)
  (evil-mode 1)
  (evil-set-initial-state 'vterm-mode 'emacs))

(setq corfu-auto t)
(setq corfu-auto-prefix 1)

(use-package corfu
 :straight t
  :init
  (global-corfu-mode))


(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
			   `(go-ts-mode . ("gopls"))))

(add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)

(set-face-attribute 'default nil :height 160)

(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(set-frame-font "ubuntu mono" nil t)

(use-package edraw
  :straight (:type git :host github :repo "misohena/el-easydraw"))

(with-eval-after-load 'evil
  ;; Navigation
  (define-key evil-normal-state-map (kbd "gd") #'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "gD") #'xref-find-definitions-other-window)
  (define-key evil-normal-state-map (kbd "gr") #'xref-find-references)
  (define-key evil-normal-state-map (kbd "gi") #'eglot-find-implementation)

  ;; Documentation
  (define-key evil-normal-state-map (kbd "K") #'eldoc-print-current-symbol-info)

  ;; Rename
  (define-key evil-normal-state-map (kbd "gR") #'eglot-rename)

  ;; Code actions
  (define-key evil-normal-state-map (kbd "ga") #'eglot-code-actions)

  ;; Format
  (define-key evil-normal-state-map (kbd "gf") #'eglot-format))

(setq major-mode-remap-alist
      '((go-mode . go-ts-mode)
        (python-mode . python-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
        (bash-mode . bash-ts-mode)
        (js-mode . js-ts-mode)
        (typescript-mode . tsx-ts-mode)
        (json-mode . json-ts-mode)
        (yaml-mode . yaml-ts-mode)))

(use-package consult
  :straight t)

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)))

;;;; Evil

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

;;;; Git

(use-package magit
  :straight t)

;;;; Useful Consult bindings

(global-set-key (kbd "M-s g") #'consult-ripgrep)
(global-set-key (kbd "M-s l") #'consult-line)

;;;; Better minibuffer completion

(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

;;;; Corfu

(setq corfu-auto t
      corfu-auto-prefix 1
      corfu-cycle t)

;;;; Embark + Consult integration

(with-eval-after-load 'embark
  (require 'consult))

(global-auto-revert-mode 1)

;; somne functons
(defun open-config ()
  (interactive)
  (find-file user-init-file))

;; my extra basic keybindings
;; core
(global-set-key (kbd "C-x i") #'open-config)
(global-set-key (kbd "C-x e") #'eval-buffer)
(global-set-key (kbd "C-x r") #'eval-region)
;; user
(global-set-key (kbd "C-c h") #'windmove-left)
(global-set-key (kbd "C-c j") #'windmove-down)
(global-set-key (kbd "C-c k") #'windmove-up)
(global-set-key (kbd "C-c l") #'windmove-right)


;; lsp
(define-prefix-command 'lsp)
(global-set-key (kbd "C-c l") 'lsp)

(global-set-key (kbd "C-c l s") #'consult-imenu)
(defun my/workspace-symbols ()
  (interactive)
  (if (and (fboundp 'eglot-workspace-symbols)
           (eglot-current-server))
      (condition-case err
          (call-interactively #'eglot-workspace-symbols)
        (error
         (message "Eglot workspace symbols failed: %s; using Consult"
                  (error-message-string err))
         (call-interactively #'consult-imenu-multi)))
    (call-interactively #'consult-imenu-multi)))
(global-set-key (kbd "C-c l S") #'my/workspace-symbols)

(global-set-key (kbd "C-c l r") #'eglot-rename)
(global-set-key (kbd "C-c l d") #'xref-find-definitions)
(global-set-key (kbd "C-c l f") #'xref-find-references)
(global-set-key (kbd "C-c l i") #'xref-find-implementations)


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

