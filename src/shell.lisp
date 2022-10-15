;;;; -------------------------------------------------------------------------
;;;; Functions to interact with the shell

(in-package :shell)

(defun log-to-stdout (text)
  "Logs the supplied text to stdout."
  (princ text)
  (fresh-line))

(defun execute-in-system (command-string)
  "Executes the supplied command in the underlying system."
  (uiop:run-program command-string
                    :input :interactive
                    :output :interactive
                    :error-output t
                    :ignore-error-status t))

(defun get-result-from-system (command-string)
  "Gets the result of execution of the supplied command string in the
underlying system."
  (uiop:run-program command-string
                    :output '(:string :stripped t)
                    :error-output t
                    :ignore-error-status t))

(defun get-list-from-system (command-string)
  "Executes the supplied command string in the underlying system and returns
a list."
  (string-to-list (get-result-from-system command-string)))

(defun exists-in-system-p (command-string)
  "Returns whether the supplied command exists in the underlying system."
  (not (string-equal (get-result-from-system (concatenate 'string
                                                          "command -v "
                                                          command-string))
                     "")))

(defun execute-if-exists (command arguments-as-string)
  "Executes the given command with supplied arguments, if it exists."
  (if (exists-in-system-p command)
      (execute-in-system (concatenate 'string
                                      command
                                      " "
                                      arguments-as-string))))
