(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/")))
;(add-to-list 'package-archives
;             '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; My packages:
(defvar my-packages '(color-theme
		      color-theme-monokai
)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'color-theme)
(color-theme-monokai)

(set-default-font "Inconsolata-12")
(set-fontset-font (frame-parameter nil 'font)
      'han '("Inconsolata-12" . "unicode-bmp-12"))
(add-to-list 'default-frame-alist '(font . "Inconsolata"))

(require 'sr-speedbar)
(setq speedbar-hide-button-brackets-flag t)
(setq speedbar-show-unknown-files t)
(setq speedbar-smart-directory-expand-flag t)
(setq speedbar-use-images nil)
(setq speedbar-frame-parameters
      '((minibuffer)
        (width . 40)
        (border-width . 0)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (unsplittable . t)
        (left-fringe . 0)))

(custom-set-variables
  '(column-number-mode t)
  '(display-battery-mode t)
  '(display-time-mode t)
  '(frame-background-mode (quote light))
  '(fringe-mode 0 nil (fringe))
  '(load-home-init-file t t)
  '(show-paren-mode t)
  '(line-number-mode t)
  '(xterm-mouse-mode t)
  '(pc-selection-mode t)
  '(global-font-lock-mode t)
  '(global-hl-line-mode t)
  '(tool-bar-mode nil)
  '(menu-bar-mode nil)
  '(scroll-bar-mode nil)
  '(blink-cursor-mode nil)
  '(yas/global-mode t)
  '(global-company-mode nil)
  '(speedbar-show-unknown-files t)
  '(desktop-save-mode t)
  '(sr-speedbar-right-side nil)
  '(sr-speedbar-skip-other-window-p t)
  '(sr-speedbar-max-width 20)
  '(sr-speedbar-width-x 10)
  '(sr-speedbar-width-console 40)
  '(sr-speedbar-auto-refresh nil)
 )

(sr-speedbar-open)
