;;;; -------------------------------------------------------------------------
;;;; Functions to interact with the user

(in-package :interface)

(defun notify-thru-gui (message)
  "Draws a notification message graphically."
  (execute-if-exists "notify-send"
                     (concatenate 'string
                                  " \""
                                  message
                                  "\"")))

(defun log-to-user (message)
  "Shows message in form of all possible media."
  (log-to-stdout message)
  (notify-thru-gui message))
