;;;; -------------------------------------------------------------------------
;;;; main

(in-package :main)

(defun run-check (current-battery-charge battery-threshold)
  "Runs the monitoring routine."
  (if (< current-battery-charge battery-threshold)
      (progn
        (execute-if-exists "beep" "-l 500")
        (log-to-user "Power below set threshold! The system will now be suspended.")
        (suspend-system))
      (log-to-stdout "Power looks OK.")))

(defun main ()
  "The main entry point to the program."
  (unless (get-batteries)
    (progn
      (log-to-stdout "No batteries installed on the system!")
      (uiop:quit)))
  (let* ((args (uiop:command-line-arguments))
         (battery-threshold (parse-integer (or (car args)
                                               "10")))
         (current-battery-level 50)
         (previous-battery-level 50)
         (sleep-timer 15))
    (labels ((fetch-battery-level ()
               (setf current-battery-level
                     (get-remaining-charge))) ;; Stores the current battery-level
             (adjust-timer ()
               ;; Check if the level has dropped
               (if (< current-battery-level previous-battery-level)
                   (and (setf sleep-timer 15)) ; Speed back timer
                   (and (< sleep-timer 60) (setf sleep-timer
                                                 (+ sleep-timer 5)))))) ; Slow down timer
      (loop
       (fetch-battery-level) ; Poll for remaining battery charge
       (adjust-timer) ; Adapt timer according to the current level
       (run-check current-battery-level
                  battery-threshold) ; Run check
       (setf previous-battery-level current-battery-level) ; Store back current level
       (log-to-stdout (concatenate 'string
                                   "Will check after "
                                   (write-to-string sleep-timer)
                                   " seconds..."))
       ;; Sleep for a while
       (sleep sleep-timer)))))
