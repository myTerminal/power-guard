;;;; -------------------------------------------------------------------------
;;;; Function to interact with the hardware

(in-package :hardware)

(defun get-battery-charges (batteries)
  "Get the remaining battery charges for all the batteries on the system."
  (mapcar (lambda (bat)
            (parse-integer (get-result-from-system (concatenate 'string
                                                                "cat "
                                                                bat
                                                                "/energy_now"))))
          batteries))

(defun get-battery-capacities (batteries)
  "Get the total battery capacities for all the batteries on the system."
  (mapcar (lambda (bat)
            (parse-integer (get-result-from-system (concatenate 'string
                                                                "cat "
                                                                bat
                                                                "/energy_full"))))
          batteries))

(defun get-remaining-charge ()
  "Get the total remaining battery charge on the system."
  (let* ((batteries (get-list-from-system "find /sys/class/power_supply -name \"BAT*\""))
         (remaining-charges (get-battery-charges batteries))
         (total-capacities (get-battery-capacities batteries)))
    (if (and remaining-charges total-capacities)
        (floor (* 100
                  (/ (reduce (lambda (a b)
                               (+ a b))
                             remaining-charges)
                     (reduce (lambda (a b)
                               (+ a b))
                             total-capacities))))
        100)))
