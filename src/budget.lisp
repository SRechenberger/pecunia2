(in-package :cl-user)
(defpackage budget
  (:use :cl :lucerne
        :parse-number
        )
  (:export :main)
  (:documentation "Main budget code."))
(in-package :budget)
(annot:enable-annot-syntax)


;;; App

(defapp pecunia
  :middlewares (clack.middleware.session:<clack-middleware-session>
                (clack.middleware.static:<clack-middleware-static>
                 :root (asdf:system-relative-pathname :budget #p"static/")
                 :path "/static/")))

(defun main ()
   (budget.models:configure-database)
   (start pecunia :port 8000))

;;; Templates

(djula:add-template-directory
 (asdf:system-relative-pathname :budget #p"templates/"))

(defparameter +index+ (djula:compile-template* "index.html"))
(defparameter +budgets+ (djula:compile-template* "budgets.html"))

;;; Views

(defclass budget-output ()
  ((bdg :accessor get-budget
        :initarg :budget
        :type budget.models:budget)
   (balance :accessor get-balance
            :initarg :balance
            :type single-float)
   (inits :accessor get-inits
          :initarg :inits
          :type list)
   (eventuals :accessor get-eventuals
              :initarg :eventuals
              :type list)))

@route pecunia "/"
(defview index ()
  (if (lucerne-auth:logged-in-p)
    (let ((user (current-user)))
      (render-template (+budgets+)
                       :username (budget.models:user-name user)
                       ;; Dummy values!
                       :budgets
                       (let ((budgets (budget.models:get-budgets-of-user
                                        (budget.models:user-id (current-user)))))
                         (loop :for bdg :in budgets
                               :collect (make-instance 'budget-output
                                                       :budget bdg
                                                       :balance
                                                       (budget.models:get-balance-of-budget
                                                         :budget-id
                                                         (budget.models:budget-id bdg))
                                                       :inits
                                                       (budget.models:get-init-positions-of-budget
                                                         :budget-id
                                                         (budget.models:budget-id bdg))
                                                       :eventuals
                                                       (budget.models:get-eventual-positions-of-budget
                                                         :budget-id
                                                         (budget.models:budget-id bdg)))))))
    (render-template (+index+))))

@route pecunia (:post "/add-budget")
(defview add-budget ()
  (with-params (title descr)
   (progn
    (budget.models:add-budget :title title
                              :descr descr
                              :owner (budget.models:user-id (current-user))
                              :closedp 0)))
  (redirect "/"))


@route pecunia (:post "/signup")
(defview sign-up ()
  (with-params (username password password-repeat)
    (if (string= password password-repeat)
      (handler-case
        ;; Try to add the user.
        ;; May cause a `name-taken-error`.
        (progn
          (format t "Signing up ~A~%" username)
          (budget.models:add-user username password)
          (format t "Signed up ~A~%" username)
          (redirect "/"))
        ;; on `NAME-TAKEN-ERROR`
        (budget.models:name-taken-error (err)
          (render-template (+index+)
                           :error (budget.models:taken-name err))))
      (render-template (+index+)
                       :error "Passwords do not match!"))))

@route pecunia (:post "/add-position/:budget-id/:which")
(defview add-position (which budget-id)
  (with-params (title descr value)
    (progn
      (format t "(:TITLE ~A :DESCR ~A :VALUE ~A)~%" title descr value)
      (budget.models:add-pos-to-budget :budget-id (parse-integer budget-id)
                                     :title title
                                     :descr descr
                                     :value value
                                     :timestamp (cond
                                                  ((string= which "eventual")
                                                   (format nil "~A" (local-time:now)))
                                                  ((string= which "init") nil)
                                                  (T nil)))
      (redirect "/"))))

@route pecunia (:post "/signin")
(defview sign-in ()
  (with-params (username password)
    (handler-case
      (progn
        ;; This may cause a `login-failed-error`.
        (budget.models:login username password)
        ;; This will only be executed, if no condition was caused.
        (lucerne-auth:login username)
        (redirect "/"))
      (budget.models:login-failed-error (err)
        (render-template (+index+)
                         :error (budget.models:login-failed-message err))))))

@route pecunia "/signout"
(defview sign-out ()
  (when (lucerne-auth:logged-in-p)
    (lucerne-auth:logout))
  (redirect "/"))

;;; Utils



(defun current-user ()
  "Gets the user-id of the current user."
  (let ((username (lucerne-auth:get-userid)))
    (when username
      (budget.models:find-user username))))
