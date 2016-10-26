;; -*- orgstruct-heading-prefix-regexp: ";;; " -*-

;;; * window/gui tweaks

;; to make window look nicely

(setq inhibit-startup-screen t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; this avoids blinking app icon in windows
(setq ring-bell-function 'ignore)

(add-to-list 'default-frame-alist
             '(font . "Source Code Pro-11"))

;;; * user info

(customize-set-variable 'user-full-name "Florian Hassanen")
(customize-set-variable 'user-mail-address "florian.hassanen@gmail.com")

;;; * encoding

(set-language-environment "UTF-8")
(set-locale-environment "en_US.UTF-8")
(set-coding-system-priority 'utf-8 'windows-1252-dos)
(prefer-coding-system 'utf-8-unix)
(add-to-list 'file-coding-system-alist '("\\.\\(bat\\|cmd\\)$" . cp850-dos))

(setq-default indent-tabs-mode nil)

;;; * package.el

(require 'package)
(customize-set-variable 'package-enable-at-startup nil)
(package-initialize t)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

;;; ** use-package

;; use-package is used to organize this .emacs

;; package list needs to be refreshed prior to installing anything
;; an advice is installed for `package-install' to do this once
;; when all packages are loaded, advice is removed

(setq my/package-el-refreshed nil)
(defun my/package-refresh-contents-once-advice (&rest args)
  (when (not my/package-el-refreshed)
    (package-refresh-contents)
    (setq my/package-el-refreshed t)))

(advice-add 'package-install :before 'my/package-refresh-contents-once-advice)

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(package-initialize)

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;(customize-set-variable 'use-package-verbose t)
(customize-set-variable 'use-package-always-ensure t)

;;; * emacs customize auto generated config

;;; ensure this file is not overwritten automatically
;;; customizations will be done manually, so disregard
;;; this custom-file (i.e. do not load it)
(setq custom-file "~/.emacs-custom-ignored.el")

(customize-set-variable 'custom-safe-themes
                        '("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
                          "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57"
                          default))

;;; * packages

(use-package base16-theme
  :config
  (load-theme 'base16-eighties t)
  (defvar my/base16-colors base16-eighties-colors)
  (setq evil-emacs-state-cursor   `(,(plist-get my/base16-colors :base0D) box)
        evil-insert-state-cursor  `(,(plist-get my/base16-colors :base0D) bar)
        evil-motion-state-cursor  `(,(plist-get my/base16-colors :base0E) box)
        evil-normal-state-cursor  `(,(plist-get my/base16-colors :base0B) box)
        evil-replace-state-cursor `(,(plist-get my/base16-colors :base08) bar)
        evil-visual-state-cursor  `(,(plist-get my/base16-colors :base09) box)))

(use-package evil
  :config
  (evil-mode t)

  (use-package evil-escape
    :config
    (evil-escape-mode)))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :mode ("\\.org\\'" . org-mode)
  :config
  (customize-set-variable 'org-log-note-clock-out t)
  (customize-set-variable 'org-pretty-entities t))

(use-package writeroom-mode
  :commands writeroom-mode)

(use-package helm-config
  :ensure helm
  :init
  (helm-mode 1)
  :bind (("M-x" . helm-M-x)
         :map
         global-map
         ([remap find-file] . helm-find-files)
         ([remap occur] . helm-occur)
         ([remap list-buffers] . helm-buffers-list)
         ([remap dabbrev-expand] . helm-dabbrev)))

(use-package magit
  :commands magit-status
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (defun org-sync ()
    (interactive)
    (save-buffer)
    (if (magit-git-string "status" "-uno" "--porcelain")
        (magit-call-git "commit" "-a" "--allow-empty-message" "-m" ""))
    (if (magit-git-success "pull")
        (progn
          (magit-run-git "push")
          (revert-buffer t t))
      (progn
        (magit-refresh)
        (magit-status)))))

(use-package smartparens-config
  :ensure smartparens
  :demand t
  :bind (:map
         smartparens-mode-map
         ("C-M-)" . sp-forward-slurp-sexp)
         ("C-M-0" . sp-forward-barf-sexp)
         ("C-M-(" . sp-backward-slurp-sexp)
         ("C-M-9" . sp-backward-barf-sexp))
  :config
  (smartparens-global-mode t)
  (use-package evil-smartparens
    :commands evil-smartparens-mode
    :init
    (add-hook 'smartparens-enabled-hook 'evil-smartparens-mode)))

(use-package web-mode
  :mode ("\\.html?\\'" . web-mode))

(use-package company
 :demand t
 :bind (:map
        company-active-map
        ("<return>" . nil)
        ("RET" . nil)
        ("<C-return>" . company-complete-selection)
        ("<tab>" . nil)
        ("TAB" . nil)
        ("<C-tab>" . company-complete-common-or-cycle))
 :config
 (customize-set-variable 'company-idle-delay 0.1)
 (customize-set-variable 'company-minimum-prefix-length 1)
 (global-company-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package smart-mode-line
  :config
  (customize-set-variable 'sml/theme 'respectful)
  (sml/setup))

;;; ** TODO provide some nice key-bindings for syncing org files within git
;;; ** currently unused/unconfigured

(use-package cider
  :pin melpa-stable
  :commands (cider-connect cider-jack-in)
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  ;;(setq nrepl-hide-special-buffers t)
  )

;; clj-refactor
(use-package clj-refactor
  :pin melpa-stable
  :commands clj-refactor-mode
  :config
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-a"))))

;;; * misc configuration

;;; ** remove package-refresh-contents advice

(advice-remove 'package-install 'my/package-refresh-contents-once-advice)

(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))
