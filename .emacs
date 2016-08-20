(setq inhibit-startup-screen t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq ring-bell-function 'ignore)

(customize-set-variable 'user-full-name "Florian Hassanen")
(customize-set-variable 'user-mail-address "florian.hassanen@gmail.com")

(set-language-environment "UTF-8")
(set-locale-environment "en_US.UTF-8")
(set-coding-system-priority 'utf-8 'windows-1252-dos)
(prefer-coding-system 'utf-8-unix)
(add-to-list 'file-coding-system-alist '("\\.\\(bat\\|cmd\\)$" . cp850-dos))

(setq-default indent-tabs-mode nil)

(add-to-list 'default-frame-alist
             '(font . "Source Code Pro-11"))

(require 'package)
(customize-set-variable 'package-enable-at-startup nil)
(package-initialize t)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(package-initialize)

(require 'use-package)
;(customize-set-variable 'use-package-verbose t)
(customize-set-variable 'use-package-always-ensure t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(use-package color-theme-sanityinc-tomorrow
  :config
  (customize-set-variable 'custom-safe-themes
                          '("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
                            default))
  (load-theme 'sanityinc-tomorrow-eighties t))

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
  (customize-set-variable 'org-log-note-clock-out t))

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
    (let ((commited (magit-call-git "commit" "-a" "--allow-empty-message" "-m" ""))
          (conflict (magit-call-git "pull" "--rebase")))
      (if (= 0 conflict)
          (magit-run-git-async "push")
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

; cider
;(add-hook 'cider-mode-hook #'eldoc-mode)
;(setq nrepl-hide-special-buffers t)

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

; clj-refactor
;(require 'clj-refactor)
;(add-hook 'clojure-mode-hook (lambda ()
;                               (clj-refactor-mode 1)
;                               (cljr-add-keybindings-with-prefix "C-c C-a")))

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

(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))
