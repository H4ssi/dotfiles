(setq inhibit-startup-screen t)

(menu-bar-mode -1)
(tool-bar-mode -1)

(setq-default indent-tabs-mode nil)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(defun check-and-install (package)
  (unless (package-installed-p package)
    (package-install package)))

(mapc 'check-and-install '(color-theme-sanityinc-tomorrow
                           evil
                           helm
                           magit
                           smartparens
                           evil-smartparens
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
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-eighties t)

(evil-mode t)
(require 'helm-config)
(helm-mode t)
(setq magit-last-seen-setup-instructions "1.4.0")

; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)
(define-key smartparens-mode-map (kbd "C-M-)") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-0") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-(") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-9") 'sp-backward-barf-sexp)

; evil-smartparens
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

; cider
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq nrepl-hide-special-buffers t)

; company
(add-hook 'after-init-hook 'global-company-mode)

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
(sml/setup)
