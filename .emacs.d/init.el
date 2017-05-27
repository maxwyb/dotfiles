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

;; --------------------
;; -- Configurations --
;; --------------------
;; Emacs theme settings for running in GUI or terminal
;; (use-package solarized-theme
;;   :ensure t)
(if window-system
    (load-theme 'wombat)
    ;;(load-theme 'solarized-dark)
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
;;(linum-mode 1)
;;(global-linum-mode 1)
;; linum-mode should not be enabled in docView mode to accelerate loading
(add-hook 'prog-mode-hook
	  (lambda () (linum-mode 1)))
(column-number-mode t)
(show-paren-mode 1)

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
;;(org-startup-truncated nil)
;;(set-default 'truncate-lines t)
;;(add-hook 'org-mode-hook (lambda () (toggle-truncate-lines 1)))

;; ido-mode
(require 'ido)
(ido-mode t)

;; set Emacs environment variables to be the same in shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; custom key bindings
(if (eq system-type 'darwin)
    (progn
      (global-set-key (kbd "s-[") (lambda () (interactive) (other-window -1)))
      (global-set-key (kbd "s-]") (lambda () (interactive) (other-window 1))))
  (progn
    (global-set-key (kbd "M-[") (lambda () (interactive) (other-window -1)))
    (global-set-key (kbd "M-]") (lambda () (interactive) (other-window 1)))))


;; --------------
;; -- Packages --
;; --------------
;; smooth scrolling
;;(require 'smooth-scrolling)
(use-package smooth-scrolling
  :ensure t
  :config
  (setq smooth-scroll-margin 5)
  (smooth-scrolling-mode 1))

;; Enable Emacs mouse support in iTerm2
(when (eq system-type 'darwin)
  (unless window-system
    (require 'mouse)
    (xterm-mouse-mode t)
    (global-set-key [mouse-4] (lambda ()
				(interactive)
				(scroll-down 1)))
    (global-set-key [mouse-5] (lambda ()
				(interactive)
				(scroll-up 1)))
    (defun track-mouse (e))
    (setq mouse-sel-mode t)))

;; neotree
(add-to-list 'load-path "~/.emacs.d/elpa/neotree")
;;(require 'neotree)
(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle))

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

;; tuareg-mode for OCaml
;; NOTE: has to be pre-installed by opam
(when (eq system-type 'darwin)
  (load "/Users/Max/.opam/system/share/emacs/site-lisp/tuareg-site-file"))

;; Python and elpy
(with-eval-after-load 'python
  (elpy-enable))
(use-package elpy
  :ensure t
  :commands (elpy-enable)
  :config
  (setq elpy-rpc-backend "jedi"))

;; Install irony-mode on MELPA, for C/C++ auto-completion
(use-package irony
  :ensure t
  ;;:commands 'irony-mode
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  
  :config
  ;; company-irony
  (use-package company-irony
    :ensure t
    ;; the following lines have to be run after package loads, not before
    :config
    (add-to-list 'company-backends 'company-irony))

  ;; enable company-mode
  (use-package company
    :ensure t
    :init
    (add-hook 'after-init-hook 'global-company-mode)))
