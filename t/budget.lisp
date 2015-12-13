(in-package :cl-user)
(defpackage budget-test
  (:use :cl :fiveam))
(in-package :budget-test)

(def-suite tests
  :description "budget tests.")
(in-suite tests)

(test simple-test
  (is
   (equal 1 1))
  (is-true
   (and t t)))

(run! 'tests)
