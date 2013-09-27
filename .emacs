(setq inhibit-start-screen t)
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(when (window-system)
  (menu-bar-mode -1)
  (menu-bar-mode 1))


(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; fix the PATH variable
;; OLD VERSION
;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))

;; (when window-system (set-exec-path-from-shell-PATH))

;; NEW VERSION from http://stackoverflow.com/questions/2266905/emacs-is-ignoring-my-path-when-it-runs-a-compile-command
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell 
         (replace-regexp-in-string "[[:space:]\n]*$" "" 
                                   (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

(setq default-frame-alist
      '((top . 75) (left . 150)
        (width . 132) (height . 55)
        (cursor-color . "white")
        (cursor-type . box)
	(font . "-apple-Source_Code_Pro-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")))

;; NEVER use tabs, ALWAYS use spaces.  Maybe there is a file somewhere where I want to use a tab,
;; but if so I haven't seen it.
(setq-default indent-tabs-mode nil)

;; instead of foo.rb<2>, call a second buffer lib/foo.rb
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Rinari
(add-to-list 'load-path "~/.emacs.d/rinari/")
(require 'rinari)

;; rspec mode
(add-to-list 'load-path "~/.emacs.d/rspec-mode/")
(require 'rspec-mode)
(setq rspec-use-rake-flag nil)

;; per github page, zsh doesn't play nicely, so use bash instead.
(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))
(ad-activate 'rspec-compile)

;; rvm
(add-to-list 'load-path "~/.emacs.d/rvm.el/")
(require 'rvm)
(rvm-autodetect-ruby)

;; cucumber
(add-to-list 'load-path "~/.emacs.d/cucumber.el/")
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
(setq feature-use-rvm t)
(setq feature-cucumber-command "bundle exec cucumber \"{feature}\" {options} --require features")

;; markdown mode

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown" . markdown-mode))


(add-hook 'markdown-mode-hook
          '(lambda ()
             ;; set fill mode, set fill column to 80
             (auto-fill-mode)
             (set-fill-column 80)))

;; enable syntax checking in ruby.
(require 'flymake)
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(add-hook 'ruby-mode-hook
          '(lambda ()

	     ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
	     (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
		 (flymake-mode))
	     ))

(require 'package)
(add-to-list 'package-archives 
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/"))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "293f58e2d131258eb256e15545ec48724f580dc47cab7dd08330ec61430ceff3" default)))
 '(org-agenda-files (quote ("~/Dropbox/idonethis.org"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(if window-system
    (load-theme 'zenburn t))

(add-hook 'ruby-mode-hook
	  (lambda()
	    (add-hook 'local-write-file-hooks
		      '(lambda()
			 (save-excursion
			   (untabify (point-min) (point-max))
			   (delete-trailing-whitespace)
			   )))
	    (set (make-local-variable 'tab-width) 2)
	    (imenu-add-to-menubar "IMENU")
	    (define-key ruby-mode-map "\C-m" 'newline-and-indent)
	    (local-set-key "\r" 'newline-and-indent)))

(setq ruby-deep-indent-paren nil)

(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.cljs" . clojure-mode))            
(add-hook 'clojure-mode-hook
	  (lambda()
            (define-key clojure-mode-map "\C-m" 'newline-and-indent)
	    (local-set-key "\r" 'newline-and-indent)))


(setq autosave-dir "~/.emacs.d/autosaves/")
;; (make-directory autosave-dir)
(setq backup-directory-alist
      `((".*" . ,autosave-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,autosave-dir t)))

;; disable beeping during C-g and other things where the beeping
;; is dumb or annoying or both		
(setq ring-bell-function 
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort 
			abort-recursive-edit 
			exit-minibuffer
			keyboard-quit
			mwheel-scroll 
			down 
			up
			next-line 
			previous-line
			backward-char 
			forward-char))
          (ding))))

;; Improved end-of-buffer scrolling
(global-set-key "\C-v"
		(lambda () (interactive)
		  (condition-case nil (scroll-up)
		    (end-of-buffer (goto-char (point-max))))))

(global-set-key "\M-v"
		(lambda () (interactive)
		  (condition-case nil (scroll-down)
		    (beginning-of-buffer (goto-char (point-min))))))

(set-scroll-bar-mode nil)
(size-indication-mode)
(column-number-mode)

;; rainbow parens (and more?)
(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; c mode the way it should be
(setq c-basic-offset 4)

(defun google-it ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Just Friggin Google It: ")))))

;; replace zap-to-char with zap-up-to-char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.
  
  \(fn arg char)"
  'interactive)

(global-set-key "\M-z" 'zap-up-to-char)

;;; rhtml-mode
(add-to-list 'load-path "~/.emacs.d/rhtml")
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
	  (lambda () (rinari-launch)))

;; automatically revert unchanged buffers when they change on disk (like with git)
(global-auto-revert-mode 1)

;;helm
(add-to-list 'load-path "~/.emacs.d/helm/")
(require 'helm-config)

(push "~/.emacs.d/helm-cmd-t" load-path)
(require 'helm-config)
(require 'helm-cmd-t)

(global-set-key (kbd "M-t") 'helm-cmd-t)

(require 'helm-files)
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)
(setq helm-c-locate-command "mdfind -name %s %s")
(loop for ext in '("\\.swf$" "\\.elc$" "\\.pyc$")
      do (add-to-list 'helm-c-boring-file-regexp-list ext))
;;(global-set-key (kbd "M-t") 'helm-for-files)

;; ack
;;(add-to-list 'load-path "/path/to/ack-and-a-half")
(require 'ack-and-a-half)
;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;;scss customization
(add-hook 'scss-mode-hook
          (lambda()
            (setq css-indent-offset 2)
            (setq scss-compile-at-save nil)))
