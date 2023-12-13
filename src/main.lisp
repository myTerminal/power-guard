;;;; -------------------------------------------------------------------------
;;;; main

(in-package :main)

(defun main ()
  "The main entry point to the program."
  (let ((batteries (get-batteries)))

    ;; In case of no batteries, quit
    (unless batteries
      (progn
        (log-to-stdout "No batteries installed on the system!")
        (uiop:quit)))

    ;; Proceed with the available batteries
    (let* ((args (uiop:command-line-arguments))
           (battery-threshold (parse-integer (or (car args)
                                                 "10")))
           (current-battery-level 50)
           (previous-battery-level 50)
           (sleep-delay 15))
      (labels ((get-new-sleep-delay ()
                 (if (< current-battery-level previous-battery-level)
                     15 ; Speed back timer
                     (if (< sleep-delay 60)
                         (+ sleep-delay 5)
                         sleep-delay))) ; Slow down timer
               (run-check (batteries current-battery-charge battery-threshold)
                 (if (and (< current-battery-charge battery-threshold)
                          (not (ac-power-connected-p)))
                     (if (cdr batteries)
                         (hibernate-system)
                         (suspend-system))
                     (log-to-system "Power looks OK."))))
        (loop
         ;; Read remaining battery charge
         (setf current-battery-level
               (get-remaining-charge))
         ;; Perform check
         (run-check batteries
                    current-battery-level
                    battery-threshold)
         ;; Adapt timer according to the current level
         (setf sleep-delay
               (get-new-sleep-delay))
         ;; Record current battery level for future reference
         (setf previous-battery-level current-battery-level)
         ;; Log about the next check
         (log-to-system (concatenate 'string
                                     "Will check after "
                                     (write-to-string sleep-delay)
                                     " seconds..."))
         ;; Sleep until the next check
         (sleep sleep-delay))))))
