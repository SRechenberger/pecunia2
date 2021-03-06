(defsystem budget-test
  :author "Sascha Rechenberger <sascha.rechenberger@uni-ulm.de>"
  :license "GPLv3"
  :description "Tests for budget."
  :depends-on (:pecunia2
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "budget")))))
