;;; init.el --- Summary: minimal emacs config for basic editing
;;; Commentary:
;;; Emacs lite config, sane defaults and fast startup
;;; It should be easy to setup and use.
;;; Code:

(defvar emacs-packages
  '(smartparens              ;; make parenthesis editting easier
    undo-tree                ;; C-x u, super undo system
    flycheck                 ;; ultra linter package
    company                  ;; autocompletion system
    ;; neotree                  ;; neotree C-x t (file tree on side)
    treemacs
    treemacs-projectile
    projectile
    magit                    ;; ultra magic git interface
    whitespace-cleanup-mode  ;; killing trailing whitespaces
    crux                     ;; utils
    ;;    modus-vivendi-theme      ;; nice dark theme
    kaolin-themes
    lsp-mode
    lsp-ui
    dap-mode
    lsp-treemacs
    helm
    helm-projectile
    helm-descbinds
    pyvenv
    simple-modeline
    load-env-vars
    ))



(progn ;; set custom-file properly
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file t))

(progn ;; setup lsp
  (require 'package)
  (if (eq system-type 'windows-nt)
      (add-to-list 'package-archives
           '("melpa" . "http://melpa.org/packages/") t)
    (add-to-list 'package-archives
         '("melpa" . "https://melpa.org/packages/") t))

  (package-initialize))

(progn ;; install not installed packages
  (setq package-selected-packages (append emacs-packages package-selected-packages))
  (let ((not-installed-packages (seq-filter (lambda (p)
                                              (not (package-installed-p p)))
                                            emacs-packages)))
    (when not-installed-packages
      (package-refresh-contents)
      (mapc #'package-install not-installed-packages))))


(progn ;; confs
  (progn ;; smartparens
    ;; smart pairing for all
    (require 'smartparens-config)
    (setq sp-base-key-bindings 'paredit)
    (setq sp-autoskip-closing-pair 'always)
    (setq sp-hybrid-kill-entire-symbol nil)
    (sp-use-paredit-bindings)
    (show-smartparens-global-mode +1)
    (smartparens-global-mode +1)
    ;; avoid edit weirdness at M-: eval expression
    (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
    )

  (progn ;; company
    (require 'company)
    (global-company-mode +1)
    (define-key company-mode-map [C-tab] 'company-complete)
    (define-key company-active-map [C-tab] 'company-complete-common-or-cycle)
    (define-key company-active-map [tab] 'company-complete-selection)
    (define-key company-active-map (kbd "\C-n") 'company-select-next)
    (define-key company-active-map (kbd "\C-p") 'company-select-previous)
    (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "M-.") 'company-show-location))
  (progn ;; whitespace
    (require 'whitespace)
    (setq whitespace-style '(face tabs empty trailing lines-tail))
    (global-whitespace-mode +1))


  (progn ;; org-mode
    (require 'org-mouse) ;; mouse support
    )
  (progn ;;projectile
    (require 'projectile)
    (define-key projectile-mode-map (kbd "C-c p") projectile-command-map)
    (global-set-key (kbd "<f9>") 'projectile-compile-project)
    (projectile-mode +1)
    )

  (progn ;;helm
    (define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
    (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
    (global-set-key (kbd "C-c f") 'helm-recentf)
    (global-set-key (kbd "C-h C-l") 'helm-locate-library)
    (global-set-key (kbd "C-h f") 'helm-apropos)
    (global-set-key (kbd "C-h r") 'helm-info-emacs)
    (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "C-x C-m") 'helm-M-x)
    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-set-key (kbd "C-x C-d") #'helm-browse-project)
    (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (helm-descbinds-mode +1)
    (helm-mode +1)
    ;; enable Helm version of Projectile with replacment commands
    (helm-projectile-on)
)

  (progn ;; custom keybindings
    (global-set-key (kbd "C-x C-b") 'ibuffer)
    (global-set-key (kbd "M-p") 'package-install)
    (global-set-key (kbd "C-x t") 'treemacs)
    (global-set-key (kbd "C-x g") 'magit)
    (global-set-key (kbd "C-c t") 'ansi-term)
    (global-set-key (kbd "C-c d")
                    'crux-duplicate-current-line-or-region)

    ;; zoom in/out
    (global-set-key (kbd "C-+") 'text-scale-increase)
    (global-set-key (kbd "C-<pause>") 'text-scale-decrease)
    (global-set-key (kbd "C--") 'text-scale-decrease)

    (global-set-key [f8] (lambda ()
                           (interactive)
                           (find-file (expand-file-name "init.el"
                                                        user-emacs-directory))))
    (global-set-key (kbd "M-N") 'display-line-numbers-mode)
    ;; split keybindings
    ;;    (global-set-key (kbd "C-x /") 'lerax-toggle-window-split)
    (global-set-key (kbd "C-x |") 'split-window-horizontally)
    (global-set-key (kbd "C-x _") 'split-window-vertically)

    (global-set-key (kbd "C-M-S-k") 'kill-this-buffer-and-window)
    (defun kill-this-buffer-and-window ()
      (interactive)
      (if (> (length (window-list)) 1)
          (kill-buffer-and-window)
        (kill-buffer (current-buffer))))
    (global-set-key (kbd "C-<backspace>") 'crux-kill-line-backwards)
    )

  ;; lsp
  (progn
    (require 'pyvenv)
    (defun lerax-python-venv-auto-activate ()
      (interactive)
      (let* ((basepath (or (projectile-project-root) default-directory))
             (venvpath (concat basepath ".venv/")))
        (when (and (not (string-equal venvpath pyvenv-virtual-env))
               (file-exists-p venvpath))
          (pyvenv-activate venvpath))))

    (require 'lsp-mode)
    (setq lsp-keymap-prefix "C-.")
    (require 'lsp-pylsp)
    (add-hook 'python-mode-hook
              (lambda ()
                (interactive)
                (lerax-python-venv-auto-activate)
                (lsp-mode +1)
                (lsp)
                )
              ))

  ;; extra modes
  (global-undo-tree-mode +1)
  (global-display-line-numbers-mode +1)
  (global-flycheck-mode +1)
  (delete-selection-mode +1)
  (global-whitespace-cleanup-mode +1)

  ;; sane defaults
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq confirm-nonexistent-file-or-buffer nil)
  (setq x-select-enable-clipboard-manager nil)
  (setq tab-always-indent 'complete) ;; smart tab behavior - indent or complete
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil) ;; no tabs! whitespace rules
  (setq inhibit-startup-screen t)     ;; annoying startup screen
  (setq auto-save-default nil)        ;; annoying files.txt~
  (setq make-backup-files nil)        ;; annoying #files.txt#
  (menu-bar-mode -1)                  ;; annoying menu-bar
  (tool-bar-mode -1)                  ;; annoying tool-bar
  (scroll-bar-mode -1)                ;; annoying scroll-bar
  (load-theme 'kaolin-ocean)         ;; nice theme
  (blink-cursor-mode -1)              ;; unecessary

  ;; use shift + arrow keys to switch between visible buffers
  (require 'windmove)
  (windmove-default-keybindings)

  (require 'magit)
  ;; make git-commit available
  (simple-modeline-mode +1)
  )

(defun load-init-env-if-exists ()
  (require 'load-env-vars)
  (let ((env-file-path (expand-file-name ".env" user-emacs-directory)))
    (if (file-exists-p env-file-path)
        (progn
          (message "Loading %s env file..." env-file-path)
          (load-env-vars env-file-path)))))

(load-init-env-if-exists)
(provide 'init)
;;; init.el ends here
