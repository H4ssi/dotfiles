(setq inhibit-startup-screen t)

(menu-bar-mode -1)
(tool-bar-mode -1)

(setq-default indent-tabs-mode nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defun check-and-install (package)
  (unless (package-installed-p package)
    (package-install package)))

(mapc 'check-and-install '(evil
                           helm
                           magit
                           smartparens
                           evil-smartparens
                           clojure-mode
                           cider
                           company
                           clj-refactor
                           rainbow-delimiters
                           smart-mode-line))

(evil-mode t)
(require 'helm-config)
(helm-mode t)
(setq magit-last-seen-setup-instructions "1.4.0")
(require 'smartparens-config)
(smartparens-global-mode t)

; company
(add-hook 'after-init-hook 'global-company-mode)

(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-a")))
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(sml/setup)
