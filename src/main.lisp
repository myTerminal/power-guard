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
      (labels ((charge-low-p (current-charge charge-threshold)
                 (and (not (ac-power-connected-p))
                      (< current-charge charge-threshold)))
               (get-new-sleep-delay (c p)
                 (cond ((>= (- p c) 3) 10) ; Charge dropped by 3% or more
                       ((< c 20) 10) ; Charge is below 20%
                       ((>= c 100) 50) ; Battery if fully charged
                       (t (round (/ c 2))))))
        (loop
         ;; Read remaining battery charge
         (setf current-charge
               (get-remaining-charge))
         ;; Perform check, halt if needed
         (if (charge-low-p current-charge
                           charge-threshold)
             (halt-system batteries)
             (log-to-system "Power looks OK."))
         ;; Adapt timer according to the current level
         (setf sleep-delay
               (get-new-sleep-delay current-charge
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
