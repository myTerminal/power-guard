;;;; -------------------------------------------------------------------------
;;;; Functions to interact with the user

(in-package :interface)

(defun log-to-user (message)
  "Shows message in form of all possible media."
  (log-to-stdout message)
  (execute-if-exists "notify-send"
                     (concatenate 'string
                                  " \""
                                  message
                                  "\"")))
