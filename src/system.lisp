;;;; -------------------------------------------------------------------------
;;;; Functions to interact with the operating-system

(in-package :system)

(defun suspend-system ()
  "Performs a system suspend."
  (cond ((exists-in-system-p "systemctl")
         (execute-in-system "systemctl suspend"))
        ((exists-in-system-p "loginctl")
         (execute-in-system "loginctl suspend"))
        (t (log-to-user "Could not suspend the system! Please suspend manually."))))
