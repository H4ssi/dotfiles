;; -*- orgstruct-heading-prefix-regexp: ";;; " -*-

(defmacro custom (variable value)
  `(customize-set-variable ',variable ,value))

;;; * window/gui tweaks

;; to make window look nicely

(custom inhibit-startup-screen t)
(custom inhibit-startup-echo-area-message (user-login-name))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(custom frame-resize-pixelwise t)
(custom window-resize-pixelwise t)

;; this avoids blinking app icon in windows
(custom ring-bell-function 'ignore)

(add-to-list 'default-frame-alist
             '(font . "Source Code Pro-11"))

;;; * user info

(custom user-full-name "Florian Hassanen")
(custom user-mail-address "florian.hassanen@gmail.com")

;;; * encoding

(set-language-environment "UTF-8")
(set-locale-environment "en_US.UTF-8")
(set-coding-system-priority 'utf-8 'windows-1252-dos)
(prefer-coding-system 'utf-8-unix)
(add-to-list 'file-coding-system-alist '("\\.\\(bat\\|cmd\\)$" . cp850-dos))

(setq-default indent-tabs-mode nil)

;;; * package.el

;; package-initialize happens implicitly before init.el

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

;;; ** handling auto saves

(setq backup-directory "~/.emacs-backups")
(if (not (file-exists-p backup-directory))
    (mkdir backup-directory t))
(if (not (and (file-directory-p backup-directory)
              (file-writable-p backup-directory)))
    (display-warning 'backup "Backup directory not available" :emergency))
(custom backup-directory-alist `(("." . ,backup-directory)))
(custom tramp-backup-directory-alist backup-directory-alist)
(custom vc-make-backup-files t)
(custom version-control t)
(custom kept-old-versions 3)
(custom kept-new-versions 6)
(custom delete-old-versions t)
(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))
(add-hook 'before-save-hook #'force-backup-of-buffer)
(custom backup-enable-predicate
        (lambda (name)
          (and (not (string-match-p
                     (rx (or "/dev/shm/pass."
                             (seq "." (or "pgp"
                                          "gpg") string-end))) name))
               (normal-backup-enable-predicate name))))

;;; ** use-package

;; use-package is used to organize this .emacs

;; package list needs to be refreshed prior to installing anything
;; an advice is installed for `package-install' to do this once
;; when all packages are loaded, advice is removed

(defun my/package-refresh-contents-once-advice (&rest args)
  (package-refresh-contents))
(defun my/package-refresh-contents-kill-advice (&rest args)
  (advice-remove 'package-install #'my/package-refresh-contents-once-advice)
  (advice-remove 'package-refresh-contents #'my/package-refresh-contents-kill-advice))

(advice-add 'package-install
            :before
            #'my/package-refresh-contents-once-advice)
(advice-add 'package-refresh-contents
            :before
            #'my/package-refresh-contents-kill-advice)

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

;;(custom use-package-verbose t)
(custom use-package-always-ensure t)

;;; * emacs customize auto generated config

;;; ensure this file is not overwritten automatically
;;; customizations will be done manually, so disregard
;;; this custom-file (i.e. do not load it)
(setq custom-file "~/.emacs-custom-ignored.el")

(custom custom-safe-themes
        '("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
          "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57"
          default))

;;; * packages

(use-package auto-package-update
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

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
  :custom
  (evil-echo-state nil)
  (evil-want-fine-undo t)
  (evil-want-keybinding nil) ; use evil-collection instead
  :config
  (evil-mode t))

(use-package evil-escape
  :after evil
  :config
  (evil-escape-mode))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-company-use-tng nil)
  :config
  (evil-collection-init))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-agenda-files '("~/org/"))
  (org-default-notes-file "~/org/inbox.org")
  (org-refile-use-outline-path 'file)
  (org-refile-targets '((org-agenda-files . (:maxlevel . 2))))
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-log-note-clock-out t)
  (org-duration-format 'h:mm)
  (org-pretty-entities t))

(use-package ox-latex
  :ensure nil
  :after org
  :custom
  (org-latex-minted-options '(("breaklines" "true")
                              ("breakautoindent" "true")
                              ("breakanywhere" "true")
                             ;("breakindentnchars" "2")
                              ))
  (org-latex-listings 'minted)
  :config
  (custom org-latex-pdf-process (mapcar (lambda (command)
                                          (replace-regexp-in-string (regexp-quote "%latex ")
                                                                    "%latex -shell-escape "
                                                                    command))
                                        org-latex-pdf-process))
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted")))

(use-package writeroom-mode
  :commands writeroom-mode)

(use-package helm-config
  :ensure helm
  :init
  (helm-mode 1)
  :bind (([remap execute-extended-command] . helm-M-x)
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
  (smartparens-strict-mode t)
  (smartparens-global-mode t))

(use-package evil-smartparens
  :hook (smartparens-enabled . evil-smartparens-mode))

(use-package markdown-mode
  :mode (((rx "README.md" string-end) . gfm-mode)
         ((rx ".m" (zero-or-one "ark") "d" (zero-or-one "own") string-end) . markdown-mode))
  :custom
  (markdown-command "pandoc"))

(use-package web-mode
  :mode (rx ".htm" (zero-or-one "l") string-end))

(use-package js2-mode
  :mode (rx "." (zero-or-one "m") "js" string-end)
  :interpreter "node"
  :custom
  (js-indent-level 2)
  (js2-strict-missing-semi-warning nil))

(use-package indium
  :commands (indium-connect indium-launch)
  :hook (js2-mode . indium-interaction-mode))

(use-package tide
  :ensure t
  :hook ((js2-mode . tide-setup)
         (js2-mode . tide-hl-identifier-mode)))

(use-package rjsx-mode
  :mode (rx ".jsx" string-end))

(use-package company
  :demand t
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 1)
  :config
  (global-company-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package powerline
  :config
  (powerline-center-evil-theme))

;;; ** TODO provide some nice key-bindings for syncing org files within git
;;; ** currently unused/unconfigured

(use-package cider
  :pin melpa-stable
  :commands (cider-connect cider-jack-in)
  :hook (cider-mode . eldoc-mode))
  ;;(setq nrepl-hide-special-buffers t)

;; clj-refactor
(use-package clj-refactor
  :pin melpa-stable
  :commands clj-refactor-mode
  :config
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-a"))))

;;; ** PKGBUILD

(use-package pkgbuild-mode
  :mode (rx string-start "PKGBUILD" string-end))

;;; * misc configuration

(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))
