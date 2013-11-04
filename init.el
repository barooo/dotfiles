(setq inhibit-start-screen t)
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(if (window-system)
    (menu-bar-mode 1)
  (menu-bar-mode -1))

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

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
      '((top . 0) (left . 150)
        (width . 132) (height . 50)
        (cursor-color . "white")
        (cursor-type . box)
	(font . "-apple-Source_Code_Pro-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")))

;; NEVER use tabs, ALWAYS use spaces.  Maybe there is a file somewhere where I want to use a tab,
;; but if so I haven't seen it.
(setq-default indent-tabs-mode nil)

;; fill at 78 for almost everything.
(setq-default fill-column 78)

;; instead of foo.rb<2>, call a second buffer lib/foo.rb
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; rspec mode
(setq rspec-use-rake-flag nil)
(setq rspec-use-rake-when-possible nil)

;; per github page, zsh doesn't play nicely, so use bash instead.
(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))
(ad-activate 'rspec-compile)

(setq feature-cucumber-command "CUCUMBER_OPTS=\"{options}\" bundle exec cucumber {feature} --require features")

;; markdown mode

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown" . markdown-mode))


(add-hook 'markdown-mode-hook
          '(lambda ()
             ;; set fill mode, set fill column to 80
             (auto-fill-mode)))

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
 '(custom-safe-themes (quote ("dc6c0b236bb09603babadd87329aa857e286ee36715811519d4bfe6278ee4367" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "293f58e2d131258eb256e15545ec48724f580dc47cab7dd08330ec61430ceff3" default)))
 '(org-agenda-files (quote ("~/Dropbox/orgs/grocery_list.org" "~/Dropbox/orgs/agenda.org"))))

;; mobileorg support.
(setq org-directory "~/Dropbox/orgs")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-inbox-for-pull "~/Dropbox/orgs/flagged.org")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(if window-system
    (load-theme 'zenburn t))

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/.cask/24.3.1/elpa/auto-complete-20130724.1750/dict")
(ac-config-default)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'web-mode)

(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(add-hook 'enh-ruby-mode-hook
	  (lambda()
	    (add-hook 'local-write-file-hooks
		      '(lambda()
			 (save-excursion
			   (untabify (point-min) (point-max))
			   (delete-trailing-whitespace))))
	    (define-key enh-ruby-mode-map "\C-m" 'newline-and-indent)
	    (local-set-key "\r" 'newline-and-indent)
            (robe-mode)
            (rbenv-use-corresponding)))

(setq enh-ruby-deep-indent-paren nil)

(require 'smartparens-config)
(require 'smartparens-ruby)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(web-mode)
               (sp-local-pair "<" ">")
               (sp-local-pair "<%" "%>"))

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
(show-paren-mode 1)
(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(setq c-basic-offset 4)

;; replace zap-to-char with zap-up-to-char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.
  
  \(fn arg char)"
  'interactive)

(global-set-key "\M-z" 'zap-up-to-char)

;;web mode (replaced rhtml-mode?)
(add-hook 'web-mode-hook
	  (lambda () (rinari-launch)))
(add-to-list 'auto-mode-alist '("\\.html.erb" . web-mode))

;;coffee-mode
(add-to-list 'auto-mode-alist '("\\.coffee.erb" . coffee-mode))

;; automatically revert unchanged buffers when they change on disk (like with git)
(global-auto-revert-mode 1)

;;scss customization
(add-hook 'scss-mode-hook
          (lambda()
            (setq css-indent-offset 2)
            (setq scss-compile-at-save nil)))

(require 'saveplace)
(setq-default save-place t)

;; (global-set-key (kbd "M-/") 'hippie-expand)

;; pbcopy: https://github.com/wesen/emacs/blob/master/pbcopy.el
(require 'pbcopy)
(turn-on-pbcopy)



;; ;; projectile configuration
(require 'ag)
(projectile-global-mode)
(setq projectile-completion-system 'grizzl)
(setq projectile-enable-caching t)
(if window-system 
    (global-set-key (kbd "s-t") 'projectile-find-file)
  (global-set-key (kbd "M-t") 'projectile-find-file))

(setq projectile-globally-ignored-directories
      (append projectile-globally-ignored-directories '(".git" ".bundle" "vendor" "tmp" "log")))
(setq projectile-globally-ignored-files
      (append projectile-globally-ignored-files '("*.png" "*.jpg")))
                                                        
;; (setq fiplr-ignored-globs '((directories (".git" ".svn" ".bundle" "vendor" "tmp" "log"))
;;                             (files ("*.jpg" "*.png" "*.zip" "*~"))))
