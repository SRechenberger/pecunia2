(defsystem pecunia2
  :author "Sascha Rechenberger <sascha.rechenberger@uni-ulm.de>"
  :maintainer "Sascha Rechenberger <sascha.rechenberger@uni-ulm.de>"
  :license "GPLv3"
  :version "0.1"
  :homepage "https://github.com/SRechenberger/pecunia2"
  :bug-tracker "https://github.com/SRechenberger/pecunia2/issues"
  :source-control (:git "git@github.com:SRechenberger/pecunia2.git")
  :depends-on (:lucerne
               :lucerne-auth
               :clsql
               :clsql-uffi
               :cl-pass
               :clsql-helper
               :local-time
               :parse-number)
  :defsystem-depends-on (:asdf-linguist)
  :components ((:module "src"
                :serial t
                :components
                ((:file "budget.models")
                 (:file "budget"))))
  :description "A simple Budget Manager"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op budget-test))))
