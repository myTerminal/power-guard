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
  (play-audible-warning)
  (log-to-user "Power below set threshold! The system will now be suspended.")
  (log-to-system "System needs to be suspended.")
  (cond ((exists-in-system-p "systemctl")
         (execute-in-system "systemctl suspend"))
        ((exists-in-system-p "loginctl")
         (execute-in-system "loginctl suspend"))
        (t (log-to-user "Could not suspend the system! Please suspend manually."))))

(defun hibernate-system ()
  "Performs a system hibernate."
  (play-audible-warning)
  (log-to-user "Power below set threshold! The system will now go into hibernation.")
  (log-to-system "System needs to be hibernated.")
  (cond ((exists-in-system-p "systemctl")
         (execute-in-system "systemctl hibernate"))
        ((exists-in-system-p "loginctl")
         (execute-in-system "loginctl hibernate"))
        (t (log-to-user "Could not hibernate the system! Please hibernate manually."))))

(defun halt-system (batteries)
  "Puts system to suspend or hibernate depending on the number of batteries."
  (if (cdr batteries)
      (hibernate-system)
      (suspend-system)))
