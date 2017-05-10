;; ---Pre-setup---
;; Add Emacs package archive: MELPA 
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; ---Configurations---
;; Emacs theme settings for running in GUI or terminal
(if window-system
    (load-theme 'wombat)
  ;;(setq-default set-background-color "black")
  (add-to-list 'default-frame-alist '(background-color . "black")))

;; set tab width in fundamental-mode and c-mode (self-added)
;;(setq default-tab-width 8)
;;(setq tab-width 8)
;;(setq-default c-basic-offset 4)

;; Emacs window-mode customizations (self-added)
;;(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))
;;(linum-mode 1)
(global-linum-mode 1)
(column-number-mode t)
(show-paren-mode 1)

;; Emacs org-mode for LaTeX syntax highlighting (self-added)
(eval-after-load 'org
  '(setf org-highlight-latex-and-related '(latex)))
;;(add-hook 'org-mode-hook (lambda () (toggle-truncate-lines 1)))

;; ido-mode (self-added)
(require 'ido)
(ido-mode t)

;; set Emacs environment variables to be the same in shell (self-added)
(exec-path-from-shell-initialize)


;; ---Packages---
