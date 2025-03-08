;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "FX"
      user-mail-address "fxiang@ucsd.edu")

(let* ((dpi
        (if (display-graphic-p)
            (round (/ (display-pixel-height)
                      (/ (display-mm-height) 25.4))) 96))
       (font-size (round(/ dpi 6))))
  (setq doom-font (font-spec :family "Source Code Pro" :size (+ font-size 1))
        doom-unicode-font (font-spec :family "Source Code Pro For Powerline" :size font-size)
        doom-variable-pitch-font (font-spec :family "Source Code Pro" :size font-size))
  )

;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-horizon)
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

(after! evil-escape (setq evil-escape-key-sequence "fd"))

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
(map! :v "s" #'evil-surround-region
      :n "[l" #'flycheck-previous-error
      :n "]l" #'flycheck-next-error
      :i "\C-d" #'delete-forward-char
      :i "\C-k" #'kill-line
      :i "\C-y" #'yank)

(map! :leader [tab] #'evil-switch-to-windows-last-buffer)
(map! :m [tab] 'nil)
(map! :leader "e e" #'evil-multiedit-match-all)

;; open terminal
(defun kitty () (interactive) (shell-command "kitty > /dev/null 2>&1 & disown"))
(defun projectile-kitty () (interactive)
       (let ((default-directory (projectile-acquire-root))) (kitty)))
(map! :leader "\"" #'kitty)
(map! :leader "p \"" #'projectile-kitty)

(defun my-format-buffer () (interactive)
       (if lsp-mode (lsp-format-buffer) (+format/region-or-buffer)))
(map! :n ",=" #'my-format-buffer)
(map! :mode 'c++-mode :localleader "o" #'projectile-find-other-file)
(setq flycheck-disabled-checkers '(c/c++-clang c/c++-gcc python-mypy))
(map! :leader "SPC" #'execute-extended-command)

(after! cc-mode (setq c-basic-offset 2))


;; formatter
(setq-hook! 'python-mode-hook +format-with 'black)

(defun ipdb () (interactive) (insert "import ipdb; ipdb.set_trace()"))
(map! :mode 'python-mode :localleader "d b" #'ipdb)

(defun portfolio-move-cv (plist)
  (copy-file "~/CV/main.pdf" "~/source/portfolio/org/CV.pdf" 't)
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

(setq grip-preview-use-webkit t)

(setq ztree-diff-filter-list '("^\\." "^__pycache__$"))

(setq +latex-viewers '(evince))

(after! ccls
  (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
  (set-lsp-priority! 'ccls 2)) ; optional as ccls is the default in Doom

(setq lsp-lens-enable nil)

(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.rchit\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.rahit\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.rgen\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.rmiss\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.rint\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.comp\\'" . glsl-mode))

(add-to-list 'auto-mode-alist '("\\.jsonc\\'" . json-mode))


(setq TeX-view-evince-keep-focus t)

(defconst my-cc-style
  '("linux"
    (c-offsets-alist . ((innamespace . [0])))))
(c-add-style "my-cc-style" my-cc-style)
(add-hook! (c-mode c++-mode)
  (setq c-default-style "my-cc-style"))

;; the following changes cursor shape in temrinal
(defun test-send-str-to-terminal (str)
  (unless (display-graphic-p) (send-string-to-terminal str)))
(add-hook 'evil-insert-state-entry-hook (lambda () (test-send-str-to-terminal "\033[6 q")))
(add-hook 'evil-insert-state-exit-hook (lambda () (test-send-str-to-terminal "\033[2 q")))

;; put protected variables (_) to the end
(defun python--private-lessp (x y)
  (cond
   ((and (string-prefix-p "_" x)
         (not (string-prefix-p "_" y))) nil)
   ((and (string-prefix-p "_" y)
         (not (string-prefix-p "_" x))) t)
   (t (string-lessp x y))))
;; (defun company-transform-python (candidates)
;;   (seq-sort-by 'company-strip-prefix 'python--private-lessp
;;                candidates))
;; (add-hook 'python-mode-hook
;;           (lambda () (setq-local company-transformers '(company-transform-python company-sort-by-occurrence))))

(map! :mode 'python-mode :prefix "C-c" "'" #'+python/open-ipython-repl)

(map! :leader "p '" #'+vterm/toggle)
(map! :leader "'" (lambda (_) (interactive "P") (+vterm/toggle ".")))

(defun add-to-thing-at-point () (interactive)
       (let ((number (thing-at-point 'number))
             (bound (bounds-of-thing-at-point 'number)))
         (delete-region (car bound) (cdr bound))
         (insert (number-to-string (+ number current-prefix-arg)))))
(map! :leader "+" #'add-to-thing-at-point)

(setq flycheck-indication-mode nil)

(add-hook 'lsp-treemacs-generic-mode-hook 'centaur-tabs-local-mode)

;; ROS
(after! anaconda-mode
  (add-to-list 'python-shell-extra-pythonpaths "/home/fx/source/warp")
  (add-to-list 'python-shell-extra-pythonpaths "/home/fx/source/ros2/src/install/rclpy/lib/python3.12/site-packages")
  (add-to-list 'python-shell-extra-pythonpaths "/home/fx/source/ros2/src/install/std_msgs/lib/python3.12/site-packages")
  (add-to-list 'python-shell-extra-pythonpaths "/home/fx/source/ros2/src/install/sensor_msgs/lib/python3.12/site-packages")
  (add-to-list 'python-shell-extra-pythonpaths "/home/fx/source/ros2/src/install/rosgraph_msgs/lib/python3.12/site-packages")
  (add-to-list 'python-shell-extra-pythonpaths "/home/fx/source/ros2/src/install/trajectory_msgs/lib/python3.12/site-packages")
  (add-to-list 'python-shell-extra-pythonpaths "/home/fx/source/ros2/src/install/control_msgs/lib/python3.12/site-packages"))

;; web dev
(add-hook 'vue-mode-hook #'lsp!)
(setq mmm-submode-decoration-level 0)
(setq typescript-indent-level 2)
(setq css-indent-offset 2)

(add-hook 'LaTeX-mode-hook (lambda () (setq TeX-master (concat (projectile-project-root) "main.tex"))))

(defun dragon-drop () (interactive) (shell-command (concat "dragon-drop " (shell-quote-argument (buffer-file-name))) ))

(map! :n "gh" #'lsp-ui-doc-show)

;; do not watch files in .gitignore
(defun ++git-ignore-p (path)
  (let* (; trailing / breaks git check-ignore if path is a symlink:
         (path (directory-file-name path))
         (default-directory (file-name-directory path))
         (relpath (file-name-nondirectory path))
         (cmd (format "git check-ignore '%s'" relpath))
         (status (call-process-shell-command cmd)))
    (eq status 0)))
(defun ++lsp--path-is-watchable-directory-a
    (fn path dir ignored-directories)
  (and (not (++git-ignore-p (f-join dir path)))
       (funcall fn path dir ignored-directories)))
(advice-add 'lsp--path-is-watchable-directory
            :around #'++lsp--path-is-watchable-directory-a)

;; (setq lsp-rust-features ["assimp"])
