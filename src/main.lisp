;;;; -------------------------------------------------------------------------
;;;; main

(in-package :main)

(defun run-check (battery-threshold)
  "Runs the monitoring routine."
  (if (< (get-battery-level) (parse-integer battery-threshold))
      (progn
        (princ "Power below set threshold! Suspending...")
        (fresh-line)
        (execute-in-system "loginctl suspend"))
      (progn
        (princ "Power looks OK.")
        (fresh-line))))

(defun main ()
  "The main entry point to the program."
  (let* ((args (uiop:command-line-arguments))
         (battery-threshold (or (car args)
                                "10")))
    (loop
     (run-check battery-threshold)
     (sleep 20))))
