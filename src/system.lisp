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

(defun suspend-system ()
  "Performs a system suspend."
  (or (execute-if-exists "systemctl" "suspend")
      (execute-if-exists "loginctl" "suspend")
      (log-to-user "Could not suspend the system! Please suspend manually.")))
