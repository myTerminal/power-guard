;;;; -------------------------------------------------------------------------
;;;; hardware

(in-package :hardware)

(defun get-battery-level ()
  "Get the remaining battery charger at a partcular instant of time."
  (let ((energy-now (parse-integer (get-result-from-system "cat /sys/class/power_supply/BAT0/energy_now")))
        (energy-full (parse-integer (get-result-from-system "cat /sys/class/power_supply/BAT0/energy_full"))))
    (rationalize (* 100 (/ energy-now energy-full)))))
