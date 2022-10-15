;;;; -------------------------------------------------------------------------
;;;; main

(in-package :main)

(defun run-check (battery-threshold)
  "Runs the monitoring routine."
  (if (< (get-remaining-charge) (parse-integer battery-threshold))
      (progn
        (log-to-stdout "Power below set threshold! Suspending...")
        (execute-if-exists "beep" "-l 500")
        (execute-if-exists "notify-send" "\"Power below set threshold!\" \"The system will now be suspended.\"")
        (suspend-system))
      (log-to-stdout "Power looks OK.")))

(defun main ()
  "The main entry point to the program."
  (let* ((args (uiop:command-line-arguments))
         (battery-threshold (or (car args)
                                "10")))
    (loop
     (run-check battery-threshold)
     (sleep 20))))
