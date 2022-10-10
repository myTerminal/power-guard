(defpackage :utils
  (:use :cl)
  (:export :string-to-list))

(defpackage :shell
  (:use :cl)
  (:import-from :utils
                :string-to-list)
  (:export :execute-in-system
           :get-result-from-system
           :get-list-from-system))

(defpackage :hardware
  (:use :cl)
  (:import-from :shell
                :get-result-from-system
                :get-list-from-system)
  (:export :get-remaining-charge))

(defpackage :main
  (:use :cl)
  (:import-from :shell
                :execute-in-system)
  (:import-from :hardware
                :get-remaining-charge)
  (:export :main))
