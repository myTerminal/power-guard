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
  (or (execute-if-exists "systemctl" "suspend")
      (execute-if-exists "loginctl" "suspend")
      (log-to-user "Could not suspend the system! Please suspend manually.")))
