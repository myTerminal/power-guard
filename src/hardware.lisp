;;;; -------------------------------------------------------------------------
;;;; Function to interact with the hardware

(in-package :hardware)

(define-condition expected-numeric-charge-value (error)
  ((battery :initarg :battery :initform nil :reader battery-read)
   (value :initarg :value :initform nil :reader value-read))
  (:documentation "This should be signalled with a failed attempt to use a charge value as an integer.")
  (:report (lambda (condition stream)
             (format stream "Failed attempt to use charge value ~A from battery ~S."
                     (value-read condition)
                     (battery-read condition)))))

(defun get-batteries ()
  "Gets a list of batteries installed on the system."
  (get-list-from-system "find /sys/class/power_supply -name \"BAT*\""))

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
  (let* ((batteries (get-batteries))
         (remaining-charges (get-battery-charges batteries))
         (total-capacities (get-battery-capacities batteries)))
    (and remaining-charges
         total-capacities
         (floor (* 100
                   (/ (reduce (lambda (a b)
                                (+ a b))
                              remaining-charges)
                      (reduce (lambda (a b)
                                (+ a b))
                              total-capacities)))))))

(defun ac-power-connected-p ()
  "Gets whether the AC power has been connected to the system."
  (string-equal (get-result-from-system "cat /sys/class/power_supply/AC/online")
                "1"))
