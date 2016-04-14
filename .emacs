(setq inhibit-startup-screen t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-language-environment "UTF-8")
(set-locale-environment "en_US.UTF-8")
(prefer-coding-system 'utf-8-unix)

(setq-default indent-tabs-mode nil)

(add-to-list 'default-frame-alist
             '(font . "Source Code Pro"))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(require 'cl-lib)
(defun my-packages (packages)
 (let ((installed (mapcar #'package-installed-p packages)))
  (when (cl-some #'not installed)
   (package-refresh-contents)
   (cl-mapc (lambda (i p) (unless i (package-install p))) installed packages))))

(my-packages '(color-theme-sanityinc-tomorrow
               evil
               evil-escape
               org
               evil-org
               helm
               magit
               projectile
               helm-projectile
               smartparens
               evil-smartparens
               web-mode
               clojure-mode
               cider
               company
               cider-eval-sexp-fu
               clj-refactor
               rainbow-delimiters
               smart-mode-line))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-eighties t)

(require 'evil)
(evil-mode t)

(require 'evil-escape)
(evil-escape-mode)

(require 'evil-org)

(require 'helm-config)
(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-autoresize-mode 1)

(setq magit-last-seen-setup-instructions "1.4.0")

(require 'projectile)
(projectile-global-mode)

(require 'helm-projectile)
(helm-projectile-on)

; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)
(define-key smartparens-mode-map (kbd "C-M-)") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-0") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-(") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-9") 'sp-backward-barf-sexp)

; evil-smartparens
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

; cider
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq nrepl-hide-special-buffers t)

; company
(add-hook 'after-init-hook 'global-company-mode)
(customize-set-variable 'company-idle-delay 0.1)
(customize-set-variable 'company-minimum-prefix-length 1)

; cider-eval-sexp-fu
(require 'cider-eval-sexp-fu)

; clj-refactor
(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-a")))

(yas/global-mode 1)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

; smart mode line
(setq sml/theme 'respectful)
(sml/setup)
