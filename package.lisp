(defpackage :shell
  (:use :cl)
  (:export :execute-in-system
           :get-result-from-system))

(defpackage :hardware
  (:use :cl)
  (:import-from :shell
                :get-result-from-system)
  (:export :get-battery-level))

(defpackage :main
  (:use :cl)
  (:import-from :shell
                :execute-in-system)
  (:import-from :hardware
                :get-battery-level)
  (:export :main))
