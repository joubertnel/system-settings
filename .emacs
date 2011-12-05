(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;________________________________________________________________
;;;; Directories



(add-to-list 'load-path "~/.emacs.d/")
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))


(add-to-list 'load-path "~/.emacs.d/color-theme/")

;;________________________________________________________________
;;;; Initial code load

;(require 'cl)
(require 'font-lock)

(require 'color-theme)
;(require 'clojure-mode)


;;________________________________________________________________
;;;; Scheme

;(require 'quack)
;(setq scheme-program-name "csi -:c")


;;________________________________________________________________
;;;; Slime

;(setq inferior-lisp-program "/Applications/CCL/scripts/ccl64 -K utf-8")
;(add-to-list 'load-path "~/.emacs.d/slime/")
;(require 'slime)
;(setq slime-net-coding-system 'utf-8-unix)
;(slime-setup)

;;________________________________________________________________
;;;; System customizations


;; Key bindings
(defun my-lisp-mode-hook ()
  (define-key lisp-mode-map "\r" 'newline-and-indent))
;(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)


;; Allow copy/paste with other applications on the system
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Conventional mouse/arrow movement & selection
;(pc-selection-mode)
;(delete-selection-mode t)


;; Column & line numbers in mode bar
(column-number-mode t)
(line-number-mode t)
(global-linum-mode 1)


;; Colorization
(color-theme-initialize)
;(color-theme-gtk-ide)
;(color-theme-blippblopp)
;(color-theme-andreas)
;(color-theme-blue-mood)
;(color-theme-fischmeister)
;(color-theme-deep-blue)
;(color-theme-gray30)
;(color-theme-parus)
;(color-theme-charcoal-black)
;(color-theme-blue-sea)
;(color-theme-classic)
;(color-theme-dark-laptop)
(color-theme-clarity)
;(color-theme-word-perfect)

;; workaround for slow startup because of setting font in .emacs
;; https://launchpad.net/metacity/+bug/23005

;(modify-frame-parameters nil '((wait-for-wm . nil)))

;; Fontification
(global-font-lock-mode t)


;;(set-default-font "-misc-fixed-medium-r-*-*-*-140-*-*-*-*-*-*")


;(add-to-list 'initial-frame-alist
;	     '(font . "-misc-fixed-medium-r-*-*-*-140-*-*-*-*-*-*"))




;;________________________________________________________________
;;;; Programming - file types

;; JavaScript support
(autoload 'javascript-mode "javascript" "Start javascript-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(setq js-indent-level 2)

;; Handlebars support
(add-to-list 'auto-mode-alist '("\\.handlebars$" . html-mode))

;; SCSS support
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))


;; SproutCore minor mode
(add-to-list 'load-path "~/.emacs.d/sinasi")
(require 'sinasi)

;; Specify modes for Lisp file extensions
(setq auto-mode-alist (append '(("\\.emacs$" . emacs-lisp-mode)
			        ("\\.lisp$" . lisp-mode)
				("\\.cl$" . lisp-mode)
				("\\.sexp$" . lisp-mode)
			        ("\\.asd$" . lisp-mode))
			      auto-mode-alist))

;; Specify mode for Clojure file extensions
(setq auto-mode-alist (append '(("\\.clj$" . clojure-mode))
			      auto-mode-alist))

;; Specify modes for XML-type file extensions
(setq auto-mode-alist (append '(("\\.as$" . actionscript-mode)
				("\\.xml$" . nxml-mode)
				("\\.xsl$" . nxml-mode)
				("\\.rng$" . nxml-mode)
				("\\.xhtml$" . nxml-mode)
				("\\.mxml$" . nxml-mode))
			      auto-mode-alist))

;; Magic for XML Mode
(setq nxml-mode-hook
      '(lambda ()
	 (setq tab-width 2
	       indent-tabs-mode nil)
	 (set-variable 'nxml-child-indent 2)
	 (set-variable 'nxml-attribute-indent 2)))


;; Markdown
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;;________________________________________________________________
;;;; LaTeX and TeX

(setq tex-dvi-view-command "xdvi")



;;________________________________________________________________
;;;; Remote file access
(setq tramp-syntax 'url)
(require 'tramp)




;;________________________________________________________________
;;;; Keybindings 

(global-set-key [M-up] 'previous-multiframe-window)
(global-set-key [M-down] 'next-multiframe-window)
(global-set-key "\M- " 'hippie-expand)



(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(quack-fontify-style nil)
 '(quack-pretty-lambda-p t)
 '(quack-programs (quote ("csc" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
