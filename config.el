;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "FX"
      user-mail-address "fxiang@eng.ucsd.edu")

(setq doom-font (font-spec :family "Source Code Pro" :size 18)
      doom-variable-pitch-font (font-spec :family "Source Code Pro" :size 19))

(setq doom-theme 'doom-one)
(setq display-line-numbers-type 'relative)
(setq org-directory "~/org/")
(setq doom-localleader-key ",")
(setq scroll-margin 5)

;; window
(require 'window-numbering)
(window-numbering-mode t)

(map! :leader "w /" #'split-window-right)
(map! :leader "w _" #'split-window-below)
(map! :leader "w m" #'delete-other-windows)
(map! :leader "1" #'select-window-1)
(map! :leader "2" #'select-window-2)
(map! :leader "3" #'select-window-3)
(map! :leader "4" #'select-window-4)
(map! :leader "5" #'select-window-5)
(map! :leader "6" #'select-window-6)
(map! :leader "7" #'select-window-7)
(map! :leader "8" #'select-window-8)
(map! :leader "9" #'select-window-9)
(map! :leader "0" #'select-window-0)

;; tab
(setq-default tab-always-indent t)
(map! "C-}" #'centaur-tabs-forward)
(map! "C-{" #'centaur-tabs-backward)
(map! "C-M-}" #'centaur-tabs-move-current-tab-to-right)
(map! "C-M-{" #'centaur-tabs-move-current-tab-to-left)

;; evil keys
(require 'key-chord)
(key-chord-mode t)
(key-chord-define evil-insert-state-map "fd" 'evil-normal-state)

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
(map! :v "s" #'evil-surround-region
      :n "[l" #'previous-error
      :n "]l" #'next-error
      :i "\C-d" #'delete-forward-char
      :i "\C-k" #'kill-line
      :i "\C-y" #'yank)

(map! :leader [tab] #'evil-switch-to-windows-last-buffer)
(map! :m [tab] 'nil)
(map! :leader "e e" #'evil-multiedit-match-all)

;; open terminal
(defun urxvt () (interactive) (shell-command "urxvt > /dev/null 2>&1 & disown"))
(defun projectile-urxvt () (interactive)
       (let ((default-directory (projectile-acquire-root))) (urxvt)))
(map! :leader "\"" #'urxvt)
(map! :leader "p \"" #'projectile-urxvt)

(map! :n ",=" #'+format/region-or-buffer)
(map! :mode 'c++-mode :localleader "o" #'projectile-find-other-file)
(setq flycheck-disabled-checkers '(c/c++-clang c/c++-gcc python-mypy))
(map! :leader "SPC" #'execute-extended-command)

(after! cc-mode (setq c-basic-offset 2))

;; formatter
(setq +format-with-lsp nil)
(setq-hook! 'python-mode-hook +format-with 'black)

(defun ipdb () (interactive) (insert "import ipdb; ipdb.set_trace()"))
(map! :mode 'python-mode :localleader "d b" #'ipdb)

;; (after! anaconda-mode
;;   (add-to-list 'python-shell-extra-pythonpaths "/home/fx/source/warp"))

(defun portfolio-move-cv (plist)
  (copy-file "~/CV/CV.pdf" "~/source/portfolio/org/CV.pdf" 't)
  )

(defun portfolio-compile-jekyll (plist)
  (let ((default-directory "~/source/portfolio/jekyll"))
    (shell-command "bundle exec jekyll build"))
  )

(setq org-publish-project-alist
      '(
        (
         "portfolio-org"
         :base-directory "~/source/portfolio/org/"
         :base-extension "org"
         :publishing-directory "~/source/portfolio/jekyll/"
         :recursive 't
         :publishing-function org-html-publish-to-html
         :body-only 't
         :headline-levels 4
         )
        (
         "portfolio-attachment"
         :base-directory "~/source/portfolio/org/"
         :preparation-function portfolio-move-cv
         :base-extension "yml\\|html\\|scss\\|css\\|js\\|png\\|gif\\|jpg\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-function org-publish-attachment
         :publishing-directory "~/source/portfolio/jekyll/"
         :recursive 't
         )
        (
         "portfolio-server"
         :base-directory "~/source/portfolio/jekyll/_site/"
         :base-extension any
         :preparation-function portfolio-compile-jekyll
         :publishing-directory "/ssh:aws:/home/ubuntu/portfolio/"
         :publishing-function org-publish-attachment
         :recursive 't
         )
        (
         "portfolio-all"
         :components ("portfolio-org" "portfolio-attachment" "portfolio-server")
         )

        (
         "blog-org"
         :base-directory "~/blog/org/"
         :base-extension "org"
         :publishing-directory "~/blog/jekyll/"
         :recursive 't
         :publishing-function org-html-publish-to-html
         :body-only 't
         :headline-levels 4
         )
        (
         "blog-attachment"
         :base-directory "~/blog/org/_posts/assets/"
         :base-extension any
         :publishing-function org-publish-attachment
         :publishing-directory "~/blog/jekyll/assets"
         :recursive 't
         )
        (
         "blog-all"
         :components ("blog-org" "blog-attachment")
         )
        )
      )

(set-eglot-client! 'cc-mode '("ccls" "--init={'{\"clang\":{\"extraArgs\": [\"--gcc-toolchain=/usr\"]}}'"))
(setq grip-preview-use-webkit t)

(setq ztree-diff-filter-list '("^\\." "^__pycache__$"))
