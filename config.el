;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "FX"
      user-mail-address "fxiang@eng.ucsd.edu")

(setq doom-font (font-spec :family "Source Code Pro" :size 13)
      doom-variable-pitch-font (font-spec :family "Source Code Pro" :size 14))

(setq doom-theme 'doom-one)
(setq display-line-numbers-type 'relative)
(setq org-directory "~/org/")
(setq doom-localleader-key ",")
(setq scroll-margin 5)

;; window
(require 'window-numbering)
(window-numbering-mode t)

(map! :leader "w /" #'split-window-right)
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
(map! "C-}" #'centaur-tabs-forward)
(map! "C-{" #'centaur-tabs-backward)
(map! "C-M-}" #'centaur-tabs-move-current-tab-to-right)
(map! "C-M-{" #'centaur-tabs-move-current-tab-to-left)

;; evil keys
(require 'key-chord)
(key-chord-mode t)
(key-chord-define evil-insert-state-map "fd" 'evil-normal-state)

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
(evil-define-key 'visual 'global "s" 'evil-surround-region)
(evil-define-key 'insert 'global "\C-d" 'delete-forward-char)
(evil-define-key 'normal 'global "[l" #'previous-error)
(evil-define-key 'normal 'global "]l" #'next-error)
(evil-define-key 'insert 'global "\C-k" #'kill-line)
(evil-define-key 'insert 'global "\C-y" #'yank)

;; open terminal
(defun urxvt () (interactive) (shell-command "urxvt"))
(defun projectile-urxvt () (interactive)
       (let ((default-directory (projectile-acquire-root))) (urxvt)))
(map! :leader "\"" #'urxvt)
(map! :leader "p \"" #'projectile-urxvt)

(map! :mode 'c++-mode :localleader "=" #'eglot-format)
(map! :mode 'c++-mode :localleader "o" #'projectile-find-other-file)
(setq flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
(map! :leader "SPC" #'execute-extended-command)
