;;;; init.el -- Summary: minimal emacs config for basic editting
;;; Commentary: Emacs lite config, sane defaults and fast startup
;;; Code:

(defvar emacs-packages
  '(smartparens              ;; make parenthesis editting easier
    undo-tree                ;; C-x u, super undo system
    flycheck                 ;; ultra linter package
    company                  ;; autocompletion system
    company-quickhelp        ;; autocomplete docs pop-up
    neotree                  ;; neotree C-x t (file tree on side)
	magit                    ;; ultra magic git interface
	whitespace-cleanup-mode  ;; killing trailing whitespaces
    ))


(progn ;; set custom-file properly
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file))

(progn ;; setup melpa
  (require 'package)
  ;; accessing a package repo over https on Windows is a no go, so we
  ;; fallback to http there
  (if (eq system-type 'windows-nt)
      (add-to-list 'package-archives
		   '("melpa" . "http://melpa.org/packages/") t)
    (add-to-list 'package-archives
		 '("melpa" . "https://melpa.org/packages/") t))

  (package-initialize)
  (package-refresh-contents))

(progn ;; install packages
  (require 'seq)
  (mapc #'package-install
	  (seq-filter (lambda (p)
			(not (package-installed-p p)))
		      emacs-packages)))

(progn ;; confs
  (progn ;; smartparens
    (require 'smartparens-config)
    (smartparens-global-mode +1)
    (smartparens-strict-mode +1)
    (show-smartparens-mode +1))
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
  (progn ;; custom keybindings
    (global-set-key (kbd "C-x C-b") 'ibuffer)
    (global-set-key (kbd "M-p") 'package-install)
    (global-set-key (kbd "C-x t") 'neotree-toggle)
    (global-set-key (kbd "M-N") 'display-line-numbers-mode))

  ;; extra modes
  (global-undo-tree-mode +1)
  (global-display-line-numbers-mode +1)
  (global-flycheck-mode +1)
  
  ;; sane defaults
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq confirm-nonexistent-file-or-buffer nil)
  (setq-default tab-width 4)
  (setq inhibit-startup-screen t)  ;; annoying startup screen
  (setq auto-save-default nil)     ;; annoying files.txt~
  (setq make-backup-files nil)     ;; annoying #files.txt#
  (menu-bar-mode -1)               ;; annoying menu-bar
  (tool-bar-mode -1)               ;; annoying tool-bar
  )

(provide 'init)
;;; init.el ends here
