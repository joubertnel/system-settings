;; Encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


(setq make-backup-files nil)

;;________________________________________________________________
;;;; Directories

(add-to-list 'load-path "~/.emacs.d/")


;;________________________________________________________________
;;;; Initial code load

(setq package-list '(color-theme auto-complete js-comint p4 icicles js2-mode))

(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
			 ("ELPA" . "http://tromey.com/elpa/")
			 ("MELPA" . "http://melpa.milkbox.net/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))


(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))


(when (require 'color-theme nil 'noerror)
  (progn
    (color-theme-initialize)
    (color-theme-deep-blue)))


(require 'font-lock)


;;________________________________________________________________
;;;; Auto-complete
(when (require 'auto-complete-config nil 'noerror)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete-mode/dict")
  (setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
  (global-auto-complete-mode t)
  (setq ac-auto-start 2)
  (setq ac-ignore-case nil)

  )

;;________________________________________________________________
;;;; JavaScript repl
(add-to-list 'load-path "~/.emacs.d/js-comint/");
(when (require 'js-comint nil 'noerror))

(setq inferior-js-program-command "/usr/local/bin/node")
(setq inferior-js-mode-hook
      (lambda ()
	;; We like nice colors
	(ansi-color-for-comint-mode-on)
	;; Deal with some prompt nonsense
	(add-to-list 'comint-preoutput-filter-functions
		     (lambda (output)
		       (replace-regexp-in-string ".*1G\.\.\..*5G" "... "
						 (replace-regexp-in-string ".*1G.*3G" "> " output))))))




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

;; Use spaces instead of tabs for indentation, regardless of mode
(setq-default indent-tabs-mode nil)


;; Key bindings
(defun my-lisp-mode-hook ()
  (define-key lisp-mode-map "\r" 'newline-and-indent))



;; Column & line numbers in mode bar
(column-number-mode t)
(line-number-mode t)
(global-linum-mode 1)


;; Fontification
(global-font-lock-mode t)



;;________________________________________________________________
;;;; Programming - file types

;; JavaScript support
;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
;; add any symbols to a buffer-local var of acceptable global vars
;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
;; you can;t have a symbol called "someName:false"
(add-hook 'js2-post-parse-callbacks
	  (lambda ()
	    (when (> (buffer-size) 0)
	      (let ((btext (replace-regexp-in-string
			    ": *true" " "
			    (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
		(mapc (apply-partially 'add-to-list 'js2-additional-externs)
		      (split-string
		       (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
		       " *, *" t))
		))))

;; Handlebars support
(add-to-list 'auto-mode-alist '("\\.handlebars$" . html-mode))

;; SCSS support
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))


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
(setq tramp-debug-buffer t)
(require 'tramp)


;;________________________________________________________________
;;;; Keybindings 

(global-set-key "\M- " 'hippie-expand)
(global-set-key (kbd "C-x /") 'comment-region)
(global-set-key (kbd "C-x \\") 'uncomment-region)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-bounce-indent-p t)
 '(js2-enter-indents-newline t)
 '(js2-indent-on-enter-key t)
 '(quack-fontify-style nil)
 '(quack-pretty-lambda-p t)
 '(quack-programs (quote ("csc" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;________________________________________________________________
;;;; Killring / Pasteboard integration
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;; Override defaults to use the Mac copy and paste
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)


;;________________________________________________________________
;;;; Perforce
(when (require 'p4 nil 'noerror))


;;________________________________________________________________
;;;; Icicles
(when (require 'icicles nil 'noerror)
  (icy-mode 1)
  (global-set-key (kbd "C-x C-l") 'icicle-locate)
  (setq locate-command "mdfind"))
