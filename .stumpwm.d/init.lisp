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
