;; setup visual/theme preferences first.
(setq inhibit-start-screen t)
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(if (window-system)
    (menu-bar-mode 1)
  (menu-bar-mode -1))
(set-scroll-bar-mode nil)
(size-indication-mode)
(column-number-mode)

;; set up package management.
(require 'cask  "/home/vagrant/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(require 'package)
(add-to-list 'package-archives 
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/"))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(if window-system
    (load-theme 'zenburn t))

;; this looks somewhat purty, but seems to be dysfunctional.
;; (require 'powerline)
;; (if (window-system)
;;     (powerline-default-theme))

;; from http://stackoverflow.com/questions/2266905/emacs-is-ignoring-my-path-when-it-runs-a-compile-command
;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell 
;;          (replace-regexp-in-string "[[:space:]\n]*$" "" 
;;                                    (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))
;; (when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))



;; this will stop (most) of the warnings about terminal capabilities (e.g.,
;; from the rails console with inf-ruby).
(setenv "PAGER" (executable-find "cat"))

;; for guimacs, size and locate the window... er, frame?
(setq default-frame-alist
      '((top . 0) (left . 150)
        (width . 132) (height . 50)
        (cursor-color . "white")
        (cursor-type . box)
	(font . "-apple-Source_Code_Pro-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")))

;; NEVER use tabs, ALWAYS use spaces.  Maybe there is a file somewhere where I
;; want to use a tab, but if so I haven't seen it.
(setq-default indent-tabs-mode nil)

(set-fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; instead of foo.rb<2>, call a second buffer lib/foo.rb
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; use ido-style file/buffer lists
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; rspec-mode-should-not-use-rake-dammit!
(setq rspec-use-rake-flag nil)
(setq rspec-use-rake-when-possible nil)

(defadvice rspec-compile (around rspec-compile-around)
  (defun rspec-zeus-file-path ()
    (or (getenv "ZEUSSOCK")
        (concat (rspec-project-root) ".zeus.sock")))
  (defun rspec-zeus-p ()
    (and rspec-use-zeus-when-possible
         (file-exists-p (rspec-zeus-file-path))))
  ad-do-it)
(ad-activate 'rspec-compile)

(setq feature-cucumber-command "CUCUMBER_OPTS=\"{options}\" bundle exec cucumber {feature} --require features")

;; tell markdown what file extensions are markdown.
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown" . markdown-mode))

;; markdown should automatically wrap text to the fill width.
(add-hook 'markdown-mode-hook
          '(lambda ()
             (auto-fill-mode)))

;; mobileorg support.
(setq org-directory "~/Dropbox/orgs")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-inbox-for-pull "~/Dropbox/orgs/flagged.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes (quote ("d63e19a84fef5fa0341fa68814200749408ad4a321b6d9f30efc117aeaf68a2e" "26ccfd6671648911fedb90fa8279edf66baefb15e55e3db7d0849969f53b8d5d" "7fa9dc3948765d7cf3d7a289e40039c2c64abf0fad5c616453b263b601532493" "dc6c0b236bb09603babadd87329aa857e286ee36715811519d4bfe6278ee4367" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "293f58e2d131258eb256e15545ec48724f580dc47cab7dd08330ec61430ceff3" default)))
 '(org-agenda-files (quote ("~/Dropbox/orgs/grocery_list.org" "~/Dropbox/orgs/agenda.org"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; auto complete preferences, and turn on autocomplete for ruby, coffee, etc.
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/.cask/24.3.1/elpa/auto-complete-20130724.1750/dict")
(ac-config-default)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'ruby-mode)
(add-to-list 'ac-modes 'web-mode)
(add-to-list 'ac-sources 'ac-source-robe)

(autoload 'rbenv "rbenv")
(setq rbenv-executable "/usr/local/bin/rbenv")
(autoload 'global-rbenv-mode "rbenv")

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

(setq ruby-insert-encoding-magic-comment nil)
(add-hook 'ruby-mode-hook
          '(lambda ()
 	     ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
	     (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
		 (flymake-mode))
	     
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

;;smartparens, like electric but smarter.
(require 'smartparens-config)
(require 'smartparens-ruby)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(web-mode)
  (sp-local-pair "<" ">")
  (sp-local-pair "<%" "%>"))

;; make sure clojure mode is turned on for clojurescript, and
;; make sure it indents on newlines.
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.cljs" . clojure-mode))            
(add-hook 'clojure-mode-hook
	  (lambda()
            (define-key clojure-mode-map "\C-m" 'newline-and-indent)
	    (local-set-key "\r" 'newline-and-indent)))

;;yavascript stuff
(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.js.es6" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.es6" . javascript-mode))

;; autosaves should go in one place, not all the places.
(setq autosave-dir "~/.emacs.d/autosaves/")
;; (make-directory autosave-dir)
(setq backup-directory-alist
      `((".*" . ,autosave-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,autosave-dir t)))

;; STOP. THE. DAMN. BEEPING.
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

;; And scroll like a program written after 1976.
(global-set-key "\C-v"
		(lambda () (interactive)
		  (condition-case nil (scroll-up)
		    (end-of-buffer (goto-char (point-max))))))

(global-set-key "\M-v"
		(lambda () (interactive)
		  (condition-case nil (scroll-down)
		    (beginning-of-buffer (goto-char (point-min))))))


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

;; XF86(Forward|Back) are used for cycling windows, but WTF is an XF86Forward
;; key?
(if (window-system)
    (progn 
      (global-set-key (read-kbd-macro "M-<up>") 'next-multiframe-window)
      (global-set-key (read-kbd-macro "M-<down>") 'previous-multiframe-window))
  (global-set-key (read-kbd-macro "ESC <up>") 'next-multiframe-window)
  (global-set-key (read-kbd-macro "ESC <down>") 'previous-multiframe-window))

;;web mode (replaced rhtml-mode?)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(add-to-list 'auto-mode-alist '("\\.html.erb" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx" . web-mode))
(add-to-list 'auto-mode-alist '("\\.es6" . web-mode))

(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")
        ("jsx" . "\\.es6")))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))


;;coffee-mode
(add-to-list 'auto-mode-alist '("\\.coffee.erb" . coffee-mode))

;; automatically revert unchanged buffers when they change on disk (like with git)
(global-auto-revert-mode 1)

;;scss customization
(add-hook 'scss-mode-hook
          (lambda()
            (setq css-indent-offset 2)
            (setq scss-compile-at-save nil)))

;; save location in files so when you open them again you return there.
(require 'saveplace)
(setq-default save-place t)

;; pbcopy: https://github.com/wesen/emacs/blob/master/pbcopy.el
(require 'pbcopy)
(turn-on-pbcopy)

;; projectile configuration
(require 'ag)
(projectile-global-mode)
(setq projectile-completion-system 'grizzl)
(setq projectile-enable-caching t)
(if window-system 
    (global-set-key (kbd "s-t") 'projectile-find-file)
  (global-set-key (kbd "M-t") 'projectile-find-file))

;; this is supposed to just work, but it doesn't.
(setq projectile-globally-ignored-directories
      (append projectile-globally-ignored-directories '(".git"
                                                        ".bundle"
                                                        "vendor"
                                                        "tmp"
                                                        ".arcanist-extensions"
                                                        "log")))
(setq projectile-globally-ignored-files
      (append projectile-globally-ignored-files '("*.png" "*.jpg")))

;; deft
(setq deft-use-filename-as-title t)
(setq deft-text-mode 'org-mode)

(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(setq inferior-lisp-program "clisp") 

;; get rid of all Projectile ag buffers
(defun kill-all-ag-buffers ()
  (interactive)
  (dolist (b (buffer-list))
    (if (string-match "\*ag search" (buffer-name b))
        (kill-buffer b))))

(setq linum-format "%d ")

(setq evil-want-C-u-scroll t)
(setq evil-toggle-key "C-M-z")

(require 'evil)
(evil-mode 1)

(require `evil-commentary)
(evil-commentary-mode)
