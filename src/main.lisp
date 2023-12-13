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
           (charge-threshold (parse-integer (or (car args)
                                                "10")))
           (current-charge 50)
           (previous-charge 50)
           (sleep-delay 15))
      (labels ((get-new-sleep-delay (delay current-charge previous-charge)
                 (if (< current-charge previous-charge)
                     15 ; Drop delay
                     (if (< delay 60)
                         (+ delay 5)
                         delay))) ; Increase delay
               (run-check (batteries current-charge charge-threshold)
                 (if (and (< current-charge charge-threshold)
                          (not (ac-power-connected-p)))
                     (if (cdr batteries)
                         (hibernate-system)
                         (suspend-system))
                     (log-to-system "Power looks OK."))))
        (loop
         ;; Read remaining battery charge
         (setf current-charge
               (get-remaining-charge))
         ;; Perform check
         (run-check batteries
                    current-charge
                    charge-threshold)
         ;; Adapt timer according to the current level
         (setf sleep-delay
               (get-new-sleep-delay sleep-delay
                                    current-charge
                                    previous-charge))
         ;; Record current battery level for future reference
         (setf previous-charge current-charge)
         ;; Log about the next check
         (log-to-system (concatenate 'string
                                     "Will check after "
                                     (write-to-string sleep-delay)
                                     " seconds..."))
         ;; Sleep until the next check
         (sleep sleep-delay))))))
