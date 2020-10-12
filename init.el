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
    neotree                  ;; neotree C-x t (file tree on side)
    magit                    ;; ultra magic git interface
    whitespace-cleanup-mode  ;; killing trailing whitespaces
    crux                     ;; utils
    ))


(progn ;; set custom-file properly
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file t))

(progn ;; setup melpa
  (require 'package)
  (if (eq system-type 'windows-nt)
      (add-to-list 'package-archives
           '("melpa" . "http://melpa.org/packages/") t)
    (add-to-list 'package-archives
         '("melpa" . "https://melpa.org/packages/") t))

  (package-initialize))

(progn ;; install not installed packages
  (require 'seq)
  (let ((not-installed-packages (seq-filter (lambda (p)
                                              (not (package-installed-p p)))
                                            emacs-packages)))
    (when not-installed-packages
      (package-refresh-contents)
      (mapc #'package-install not-installed-packages))))


(progn ;; confs
  (progn ;; smartparens
    (require 'smartparens-config)
    (sp-use-paredit-bindings) ;; paredit keybindings
    (smartparens-global-mode +1)
    (smartparens-global-strict-mode +1)
    (show-smartparens-global-mode +1))
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

  (progn ;; recentf
    (require 'recentf)
    (recentf-mode +1)
    (setq recentf-max-menu-items 25)
    (setq recentf-max-saved-items 25)
    (global-set-key (kbd "C-c f") 'recentf-open-files))

  (progn ;; custom keybindings
    (global-set-key (kbd "C-x C-b") 'ibuffer)
    (global-set-key (kbd "M-p") 'package-install)
    (global-set-key (kbd "C-x t") 'neotree-toggle)
    (global-set-key (kbd "C-x g") 'magit)
    (global-set-key (kbd "C-c d")
                    'crux-duplicate-current-line-or-region)

    (global-set-key [f5] (lambda ()
                           (interactive)
                           (find-file (expand-file-name "init.el"
                                                        user-emacs-directory))))
    (global-set-key (kbd "M-N") 'display-line-numbers-mode))

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
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil) ;; no tabs! whitespace rules
  (setq inhibit-startup-screen t)     ;; annoying startup screen
  (setq auto-save-default nil)        ;; annoying files.txt~
  (setq make-backup-files nil)        ;; annoying #files.txt#
  (menu-bar-mode -1)                  ;; annoying menu-bar
  (tool-bar-mode -1)                  ;; annoying tool-bar
  (scroll-bar-mode -1)                ;; annoying scroll-bar
  (load-theme 'wombat)                ;; nice default theme
  (blink-cursor-mode -1)              ;; unecessary
  )

(provide 'init)
;;; init.el ends here
