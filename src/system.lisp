;;;; -------------------------------------------------------------------------
;;;; system

(in-package :system)

(defun suspend-system ()
  "Performs a system suspend."
  (execute-in-system "loginctl suspend"))
