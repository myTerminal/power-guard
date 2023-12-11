;;;; -------------------------------------------------------------------------
;;;; Functions to interact with the operating-system

(in-package :system)

(defun log-to-system (message)
  "Logs message if logger exists."
  (execute-if-exists "logger"
                     (concatenate 'string
                                  "-i -t power-guard '"
                                  message
                                  "'")))

(defun play-audible-warning ()
  "Plays audible warning."
  (execute-if-exists "beep" "-l 200 -r 3 -d 200"))

(defun suspend-system ()
  "Performs a system suspend."
  (cond ((exists-in-system-p "systemctl")
         (execute-in-system "systemctl suspend"))
        ((exists-in-system-p "loginctl")
         (execute-in-system "loginctl suspend"))
        (t (log-to-user "Could not suspend the system! Please suspend manually."))))

(defun hibernate-system ()
  "Performs a system hibernate."
  (cond ((exists-in-system-p "systemctl")
         (execute-in-system "systemctl hibernate"))
        ((exists-in-system-p "loginctl")
         (execute-in-system "loginctl hibernate"))
        (t (log-to-user "Could not hibernate the system! Please hibernate manually."))))
