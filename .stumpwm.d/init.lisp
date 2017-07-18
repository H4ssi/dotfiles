(in-package :stumpwm)

(let ((matches (directory "~/.emacs.d/elpa/slime-*/swank-loader.lisp")))
  (when matches
    (load (first matches)) 
    (funcall (find-symbol (string '#:init) (find-package :swank-loader)))
    (funcall (find-symbol (string '#:create-server) (find-package :swank)))))

(run-shell-command "dex -ae XFCE")
(run-shell-command "compton")

(define-key *root-map* (kbd "c") "exec termite")
(setf *mouse-focus-policy* :click)

(defcommand brightness-get () ()
  "Display current backlight brightness"
  (echo (run-shell-command "light -G" t)))

(defcommand brightness-up () ()
  "Increase backlight brightness"
  (run-shell-command "light -A 10")
  (brightness-get))

(defcommand brightness-down () ()
  "Decrease backlight brightness"
  (run-shell-command "light -U 10")
  (brightness-get))

(defcommand brightness-set (target)
    ((:number "Brightness (%): "))
  (run-shell-command (concatenate 'string "light -S " (write-to-string target)))
  (brightness-get))

(define-key *top-map* (kbd "XF86MonBrightnessUp") "brightness-up")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "brightness-down")
(define-key *top-map* (kbd "C-XF86MonBrightnessUp") "brightness-set 100")
(define-key *top-map* (kbd "C-XF86MonBrightnessDown") "brightness-set 1")

(define-key *root-map* (kbd "Delete") "exec i3lock -c 000000 -u -e")
(define-key *root-map* (kbd "C-Delete") "exec i3lock -c 000000 -u -e && systemctl hibernate")
(define-key *root-map* (kbd "C-M-Delete") "exec systemctl poweroff")
