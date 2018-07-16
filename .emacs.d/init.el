(setq use-package-always-ensure t)

;; ---------------
;; -- Pre-setup --
;; ---------------
;; Add Emacs package archive: MELPA 
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; elpy archive
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(package-initialize) ;; You might already have this line

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4528fb576178303ee89888e8126449341d463001cb38abe0015541eb798d8a23" default))))

;; --------------------
;; -- Configurations --
;; --------------------
;; Emacs theme settings for running in GUI or terminal
;; (use-package solarized-theme
;;   :ensure t)
(if window-system
    ;;(load-theme 'wombat)
    ;;(load-theme 'solarized-dark)
    (load-theme 'zenburn)
  ;;(setq-default set-background-color "black")
  (add-to-list 'default-frame-alist '(background-color . "black")))

;; set tab width in fundamental-mode and c-mode
;;(setq default-tab-width 8)
;;(setq tab-width 8)
;;(setq-default c-basic-offset 4)

;; Emacs window-mode customizations
(unless (eq system-type 'darwin)
  (menu-bar-mode -1))
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))
;;(global-linum-mode 1)
;; linum-mode should not be enabled in docView mode to accelerate loading
(add-hook 'prog-mode-hook
	  (lambda () (linum-mode 1)))
(column-number-mode t)
(show-paren-mode 1)

;; auto-fill-mode
(setq-default fill-column 80)

;; Emacs font settings
;;(set-face-attribute 'default nil :height 140)
(if (eq system-type 'darwin)
    (progn
      (add-to-list 'default-frame-alist
		   '(font . "Monaco-12")))
  (add-to-list 'default-frame-alist
	       '(font . "Cousine-11")))
(setq-default line-spacing 0.1)

;; Emacs org-mode for LaTeX syntax highlighting
(eval-after-load 'org
  '(setf org-highlight-latex-and-related '(latex)))
(add-hook 'org-mode-hook (lambda () (set 'truncate-lines nil)))
(set 'org-src-preserve-indentation t)
;;(org-startup-truncated nil)
;;(set-default 'truncate-lines t)
;;(add-hook 'org-mode-hook (lambda () (toggle-truncate-lines 1)))

;; LaTeX editing environment
(use-package auctex
  :ensure t
  ;; https://github.com/jwiegley/use-package/issues/417
  :defer t)

;; ido-mode
(require 'ido)
(ido-mode t)

;; re-builder for regular expressions
(setq-default reb-re-syntax 'string)

;; set Emacs environment variables to be the same in shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))
;; set PATH environment variable on Python packages for elpy
;;(setq exec-path (append exec-path '("/Users/Max/anaconda/bin")))

;; custom key bindings
(if (eq system-type 'darwin)
    (progn
      (global-set-key (kbd "s-[") (lambda () (interactive) (other-window -1)))
      (global-set-key (kbd "s-]") (lambda () (interactive) (other-window 1)))
      (global-set-key (kbd "M-RET") (lambda () (interactive) (set-mark-command nil))))
  (progn
    (global-set-key (kbd "M-[") (lambda () (interactive) (other-window -1)))
    (global-set-key (kbd "M-]") (lambda () (interactive) (other-window 1)))))
(global-set-key (kbd "s-r") (lambda () (interactive) (revert-buffer)))

;; Emacs mode-line customizations
(use-package smart-mode-line
  :ensure t)
(if window-system
    (sml/setup))

;; --------------
;; -- Packages --
;; --------------
;; smooth scrolling
;;(require 'smooth-scrolling)
;; (use-package smooth-scrolling
;;   :ensure t
;;   :config
;;   (setq smooth-scroll-margin 5)
;;   (smooth-scrolling-mode 1))
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Enable Emacs mouse support in iTerm2
;; (when (eq system-type 'darwin)
;;   (unless window-system
;;     (require 'mouse)
;;     (xterm-mouse-mode t)
;;     (global-set-key [mouse-4] (lambda ()
;; 				(interactive)
;; 				(scroll-down 1)))
;;     (global-set-key [mouse-5] (lambda ()
;; 				(interactive)
;; 				(scroll-up 1)))
;;     (defun track-mouse (e))
;;     (setq mouse-sel-mode t)))

;; neotree
;;(add-to-list 'load-path "~/.emacs.d/elpa/neotree")
;;(require 'neotree)
;; (use-package neotree
;;   :ensure t
;;   :config
;;   (global-set-key [f8] 'neotree-toggle))

;; extensions to dired
(use-package dired+
  :load-path "dired-plus"
  :config
  ;; show file details by default
  (setq diredp-hide-details-initially-flag nil)
  (setq diredp-hide-details-propagate-flag nil))

;; regular expressions
(use-package visual-regexp
  :ensure t
  :config
  (define-key global-map (kbd "C-c r") 'vr/replace)
  (define-key global-map (kbd "C-c q") 'vr/query-replace))

(use-package visual-regexp-steroids
  :ensure t
  :config
  ;; to use visual-regexp-steroids's isearch instead of the built-in regexp isearch
  (define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
  (define-key esc-map (kbd "C-s") 'vr/isearch-forward)) ;; C-M-s

;; -----------
;; -- Modes --
;; -----------
;; markdown-mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Python and elpy
;; NOTE: well-designed Lisp programs should not use with-eval-after-load
(with-eval-after-load 'python
  (elpy-enable))
;; (add-hook 'python-mode-hook 'elpy-enable)
(use-package elpy
  :ensure t
  :commands elpy-enable
  :config
  (setq elpy-rpc-backend "jedi")
  (setq elpy-rpc-python-command "python3"))

;; enable company-mode for code auto-completion interface
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;; Install irony-mode on MELPA, for C/C++ auto-completion
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(use-package irony
  :ensure t
  :commands irony-mode
  :config
  ;; company-irony
  (use-package company-irony
    :ensure t
    ;; the following lines have to be run after package loads, not before
    :config
    (add-to-list 'company-backends 'company-irony)))
 
(use-package go-mode
  :ensure t)

(use-package swift-mode
  :ensure t)

(use-package scala-mode
  :ensure t)

;; default octave-mode over objc-mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; web-mode for HTML front-end web development
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode)))

;; Modes for JavaScript IDE and React development in JSX
(use-package js2-mode
  :ensure t
  ;; install stable release from GNU ELPA
  :pin gnu
  :mode "\\.js\\'"
  :interpreter ("node" . js2-mode)
  :config
  (tern-mode t)
  (js2-mode-hide-warnings-and-errors))
(use-package rjsx-mode
  :ensure t
  :mode ("components\\/.*\\.js\\'" . rjsx-mode)
  :config
  (tern-mode t))

;; tern-mode for JavaScript integration with Emacs
;; "auto-complete" is a required package of tern
;;(use-package auto-complete
;;  :ensure t)

;; Elisp built-in autoload interferes with ":commands" of use-package
;; so let use-package manage all packages, including offline modules
;;(add-to-list 'load-path "~/.emacs.d/tern/emacs")
;;(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))

(use-package tern
  :load-path "tern/emacs"
  :commands tern-mode
  :config  
  (use-package company-tern
    :ensure t
    :config
    (add-to-list 'company-backends 'company-tern)))

;; Java IDE by emacs-eclim with eclim backend
;; only works with eclipse projects
(setq eclim-eclipse-dirs "~/eclipse/java-oxygen/Eclipse.app/Contents/MacOS/eclipse")
(setq eclim-executable "~/.p2/pool/plugins/org.eclim_2.7.1/bin/eclim")
;; (setq eclimd-autostart t)
;; (add-hook 'java-mode-hook
;; 	  (lambda ()
;; 	    (eclim-mode t)
;; 	    (setq tab-width 4)))
(use-package eclim
  :ensure t
  :commands eclim-mode
  :config
  (use-package company-emacs-eclim
    :ensure t
    :config
    (company-emacs-eclim-setup)))

;; Java IDE by meghanada-emacs
;; only supports gradle and maven projects; otherwise meghanada-server triggers
;; "java.lang.IllegalArgumentException: Project Not Found"
;; (add-hook 'java-mode-hook
;; 	  (lambda ()
;; 	    (meghanada-mode t)
;; 	    (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
;; (use-package meghanada
;;   :ensure t
;;   :commands meghanada-mode
;;   ;; company-maghanada is inside and auto-loaded
;;   )

;; [Debugging backgrace]
;; (setq max-specpdl-size 5)  ; default is 1000, reduce the backtrace level
;; (setq debug-on-error t)    ; now you should get a backtrace
