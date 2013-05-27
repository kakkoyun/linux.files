;;; Load packages ---
;; =============================================================================
(require 'sr-speedbar)

;; Hooks
;; =============================================================================
;;; Code:
(setq prelude-guru nil)

;; Custom Settings
;; =============================================================================
(custom-set-variables
 '(delete-selection-mode t)
 '(global-linum-mode t)
 '(mouse-highlight t)
 '(mouse-yank-at-point t)
 '(scroll-bar-mode nil)
 '(speedbar-show-unknown-files t)
 '(sr-speedbar-auto-refresh nil)
 '(sr-speedbar-max-width 20)
 '(sr-speedbar-right-side nil)
 '(sr-speedbar-skip-other-window-p t)
 '(sr-speedbar-width-console 40)
 '(sr-speedbar-width-x 10)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)))
 '(tab-width 4)
 '(x-select-enable-clipboard t))
;;'(tab-width )

;; Key bindigs
;; =============================================================================
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)
(global-set-key (kbd "s-r") 'sr-speedbar-refresh-toggle)
(global-set-key "\M-r" 'replace-string)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-z" 'undo)

;; Initial Modes or Minors
;; =============================================================================
(windmove-default-keybindings)
;(sr-speedbar-open)

;;(disable-theme 'zenburn)
;;(load-theme 'solarized-dark t)


;;; custom.el end here
