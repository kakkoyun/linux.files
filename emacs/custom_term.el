;; Custom Settings
;; =============================================================================
(custom-set-variables
 '(delete-selection-mode t)
 '(global-linum-mode t)
 '(mouse-highlight t)
 '(mouse-yank-at-point t)
 '(scroll-bar-mode nil)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)))
 '(tab-width 4)
 '(x-select-enable-clipboard t))

;; Key bindigs
;; =============================================================================

(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(global-set-key "\M-r" 'replace-string)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-z" 'undo)

;; Initial Modes or Minors
;; =============================================================================
(windmove-default-keybindings)


(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-screen t)

;; Packages 
;; =============================================================================

(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(package-initialize)

