(in-package :cl-user)
(defpackage budget.models
  (:use :cl
        :clsql
        :clsql-uffi
        :cl-pass)
  ;; Database Stuff
  (:export :configure-database
           :connect-to-database
           :migrate)
  ;; Budget User
  (:export :user-name
           :user-passwd
           :user-id
           :login
           :add-user
           :find-user)
  ;; Budgets
  (:export :budget
           :budget-id
           :budget-title
           :budget-descr
           :budget-closed-p
           :budget-owner
           :budget-globals
           :budget-locals
           :get-budgets-of-user
           :add-budget
           :add-pos-to-budget
           :get-all-positions-from-budget
           :get-init-positions-of-budget
           :get-eventual-positions-of-budget
           :get-balance-of-budget)
  ;; Conditions
  (:export :name-taken-error
           :taken-name
           :login-failed-error
           :login-failed-message
           ))
(in-package :budget.models)

;;;
;;; Budget User Definition
;;;

(def-view-class budget-user ()
                ((id :db-kind :key
                     :db-constraint :not-null
                     :type integer
                     :initarg :id
                     :accessor user-id
                     )
                 (name :db-constraint :not-null
                       :accessor user-name
                       :type (string 30)
                       :initarg :name)
                 (passwd :db-constraint :not-null
                         :accessor user-passwd
                         :type string
                         :initarg :passwd))
                 (:base-table budget-users))



;;;
;;; Budget User Functions
;;;
(locally-enable-sql-reader-syntax)

(define-condition name-taken-error (error)
  ((name
     :initarg :taken-name
     :accessor taken-name
     :initform nil
     :documentation "The name already taken by another user.")))

(define-condition login-failed-error (error)
  ((message
     :initarg :login-failed-message
     :accessor login-failed-message
     :initform nil
     :documentation "A message, why the login failed.")))

(defun add-user (new-name password)
  "Adds a new User, given a Username and a Password,
   if the Username is not already take."
  (print "ENTERED add-user")
  (when (connect-to-database)
    (let ((max-id (select [max [id]]
                          :from [budget_users]
                          :flatp t))
          (new-id 0))
      (if (select [name]
                  :from [budget_users]
                  :where [= [name] new-name]
                  :flatp t)
        (progn (disconnect)
               (error 'name-taken-error :taken-name new-name))
        (progn (format t "################################")
               (when (car max-id)
                 (setf new-id (+ 1 (car max-id))))
             ; (format t "######~%USERNAME: ~A~%~S~%######" max-id)
               (let ((new-user (make-instance 'budget-user
                                              :id new-id
                                              :name new-name
                                              :passwd (hash password))))
                 (update-records-from-instance new-user)
                 (disconnect)))))))

(defun login (login-name password)
  "Tries to verify a login name and a given password.
   Causes a `login-failed-error` if anything went wrong.
   Returns `NIL` on success."
  (print "ENTERED login")
  (if (connect-to-database)
    (let ((pw-hash (select [passwd]
                           :from [budget_users]
                           :where [= [name] login-name]
                           :flatp t)))
      (disconnect)
      (if pw-hash
        (unless (check-password password (car pw-hash))
          (error 'login-failed-error :login-failed-message "Password is incorrect."))
        (error 'login-failed-error
               :login-failed-message
               (format nil "Could not match user `~A` and given password." login-name))))))

(defun find-user (login-name)
  "Gets the user assoziated with the given login name.
   Returns `NIL` if the login name was not found."
  (print "ENTERED find-user")
  (if (connect-to-database)
    (let ((result (select 'budget-user
                          :from [budget_users]
                          :where [= [name] login-name]
                          :flatp t)))
      (disconnect)
      (when result
        (car result)))
    NIL))


(defun get-budgets-of-user (user-id)
  (print "ENTERED get-budgets-of-user")
  (when (connect-to-database)
    (let ((budgets (select 'budget
                           :where [= [slot-value 'budget 'owner] user-id]
                           :flatp t)))
      (disconnect)
      budgets)))

(restore-sql-reader-syntax-state)

;;;
;;; Budget
;;;

(def-view-class budget ()
  ((id :db-kind :key
       :type integer
       :db-constraint :not-null
       :accessor budget-id
       :initarg :id)
   (title :type (string 30)
          :db-constraint :not-null
          :accessor budget-title
          :initarg :title)
   (descr :type string
          :accessor budget-descr
          :initarg :descr)
   (closed :type boolean
           :initarg :closed
           :accessor budget-closed-p)
   (owner :type integer
          :db-constraint :not-null
          :accessor budget-owner
          :initarg :owner))
  (:base-table budgets))


(locally-enable-sql-reader-syntax)
(defun add-budget (&key title descr owner closedp)
  (print "ENTERED add-budget")
  (let ((to-add (make-instance 'budget
                               :title title
                               :descr descr
                               :owner owner
                               :closed-p closedp)))
    (when (connect-to-database)
      (let* ((max-id (select [max [id]]
                             :from [budgets]
                             :flatp t))
             (new-id (if (car max-id)
                       (+ (car max-id) 1)
                       0)))
        (setf (slot-value to-add 'id) new-id)
        (update-records-from-instance to-add))
      (disconnect))))

(restore-sql-reader-syntax-state)


;;; Assignment
(def-view-class assignment ()
  ((bdg-id :db-kind :key
           :type integer
           :db-constraint :not-null
           :initarg :bdg-id)
   (p-id :db-kind :key
         :type integer
         :db-constraint :not-null
         :initarg :pos-id)
   (timestamp :type string
              :initarg :date
              :initform (format nil "~A" (local-time:now))))
  (:base-table assignments))

;;; Positions

(def-view-class pos ()
  ((id :db-kind :key
       :type integer
       :db-constraint :not-null
       :accessor pos-id
       :initarg :id)
   (title :type (string 30)
          :db-constraint :not-null
          :accessor pos-title
          :initarg :title)
   (descr :type string
          :accessor pos-descr
          :initarg :descr)
   (value :type string
          :accessor pos-value
          :initarg :value
          :db-constraint :not-null)
   (tag :type integer
        :accessor pos-tag
        :initarg :tag))
  (:base-table positions))

(locally-enable-sql-reader-syntax)

(defun add-pos-to-budget (&key budget-id title descr value timestamp (tag NIL))
  (print "ENTERED add-pos-to-budget")
  (when (connect-to-database)
    (let* ((max-id (select [max [id]]
                           :from [positions]
                           :flatp t))
           (new-id (if (car max-id) (+ 1 (car max-id)) 0))
           (new-pos (make-instance 'pos
                                   :id new-id
                                   :title title
                                   :descr descr
                                   :value value
                                   :tag tag))
           (new-assgn (make-instance 'assignment
                                     :bdg-id budget-id
                                     :pos-id new-id
                                     :date timestamp)))

      (format t "(:TITLE ~A :DESCR ~A :VALUE ~A)~%" title descr value)
      (update-records-from-instance new-pos)
      (update-records-from-instance new-assgn)
      (disconnect))))

(defun get-all-positions-from-budget (&key budget-id)
  (format T "ENTERED (get-all-positions-from-budget :budget-id ~A)" budget-id)
  (when (connect-to-database)
    (let* ((p-ids (select [p-id]
                         :from [assignments]
                         :where [= [bdg-id] budget-id]
                         :flatp t
                         ))
          (poss (if p-ids
                  (select 'pos
                          :where [in [slot-value 'pos 'id]
                                      p-ids]
                          :flatp t)
                  NIL)))
      (disconnect)
      poss)))

(defun get-init-positions-of-budget (&key budget-id)
  (format T "ENTERED (get-init-positions-of-budget :budget-id ~A)" budget-id)
  (when (connect-to-database)
    (let* ((p-ids (select [p-id]
                          :from [assignments]
                          :where [and [= [bdg-id] budget-id]
                                      [is [timestamp] nil]]
                          :flatp T))
           (poss (when p-ids
                   (select 'pos
                           :where [in [slot-value 'pos 'id]
                                      p-ids]
                           :flatp T))))
      (disconnect)
      poss)))

(defun get-eventual-positions-of-budget (&key budget-id)
  (format T "ENTERED (get-init-eventual-of-budget :budget-id ~A)" budget-id)
  (when (connect-to-database)
    (let* ((p-ids (select [p-id]
                          :from [assignments]
                          :where [and [= [bdg-id] budget-id]
                                      [not [is [timestamp] nil]]]
                          :flatp T))
           (poss (when p-ids
                   (select 'pos
                           :where [in [slot-value 'pos 'id]
                                      p-ids]
                           :flatp T))))
      (disconnect)
      poss)))

(defun get-balance-of-budget (&key budget-id)
  (print "ENTERED get-balance-of-budget")
  (when T
    (let* ((ps (get-all-positions-from-budget :budget-id budget-id))
           (s (loop :for p :in ps
                    :sum (parse-number:parse-real-number (pos-value p)))))
      (print ps)
      s)))

(restore-sql-reader-syntax-state)


;;; Tags

(def-view-class tag ()
  ((id :db-kind :key
       :type integer
       :db-constraint :not-null
       :accessor tag-id
       :initarg :id)
   (title :type (string 30)
          :db-constraint :not-null
          :accessor tag-title
          :initarg :title)
   (descr :type string
          :accessor tag-title
          :initarg :title))
  (:base-table tags))

;;;
;;; Database Connection
;;;


(defvar *db-server* nil)
(defvar *db-name* nil)
(defvar *db-user* nil)
(defvar *db-password* nil)
(defvar *db-type* :mysql)

(defun configure-database (&key (server "localhost")
                                (database "budgetapp")
                                (username "budgetapp")
                                (password "budgetapp"))
  (setf *db-server* server)
  (setf *db-name* database)
  (setf *db-user* username)
  (setf *db-password* password))

(defun connect-to-database ()
  ;; Are there information about the database?
  (when (and *db-server*
             *db-name*
             *db-user*
             *db-password*)
    ;; If yes, connect!
    (connect `(,*db-server*
               ,*db-name*
               ,*db-user*
               ,*db-password*)
             :database-type *db-type*)))

(defun migrate ()
  (when (connect-to-database)
    (progn (start-sql-recording)
           (ignore-errors
             (drop-view-from-class 'assignment)
             (drop-view-from-class 'budget)
             (drop-view-from-class 'tag)
             (drop-view-from-class 'pos)
             (drop-view-from-class 'budget-user))
           (create-view-from-class 'budget-user)
           (create-view-from-class 'budget)
           (create-view-from-class 'tag)
           (create-view-from-class 'pos)
           (create-view-from-class 'assignment)
           (disconnect))))
