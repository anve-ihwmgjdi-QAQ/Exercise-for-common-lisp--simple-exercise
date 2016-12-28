(load "dynamic-change-function-cond-body.lisp")

(defvar *operator-list-save-place* (make-hash-table :test #'eq))

(defmacro make-operate-list (f-name init-list-value)
  (let ((lst-name (gensym)))
    `(let ((,lst-name ',init-list-value))
       (setf (gethash (make-package-name ',f-name) *operator-list-save-place*)
	     #'(lambda (&optional (how-do? 'get) value)
		 (declare (type list ,lst-name value))
		 (declare (type symbol how-do?))
		 (cond ((eq how-do? 'get) ,lst-name)
		       ((and value (not (listp value)))
			(error "The ~A is not of type LIST" value))
		       ((and (eq how-do? 'push)value) (push value ,lst-name))
		       ((and (eq how-do? 'pop) value) (pop value))
		       ((and (eq how-do? 'set) value) (setq ,lst-name value))
		       ((eq how-do? 'set)
			(error "You nust input a VALUE in ~A" ',f-name))
		       (t (error "Unknown type in ~A :~A" ',f-name how-do?))))))))
(make-operate-list operate-normal-oper-lst (+ - * /))
;;; special operator is like "3sin4" hide a * in the expressions
(make-operate-list operate-special-oper-lst (sin cos tan log))

(defun search-operator (string)
  (with-hash-table-iterator (next-entry *operator-list-save-place*)
    (loop (multiple-value-bind (more oper-name oper-list) (next-entry)
	    (unless more (return (values 'no-more string)))
	    (dolist (atom (funcall oper-list))
	      ;(print #\newline)
	      (let* ((oper-str (string atom))
		     (ser (search oper-str string :test #'string-equal)))
		(if ser (return-from search-operator
			  (let ((start ser)
				(end (+ ser (length oper-str))))
			    (values oper-name
				    (subseq string 0 start)
				    (subseq string start end)
				    (subseq string end)))))))))))

(defgeneric str-expr->stand-expr (string-expression)
  (:documentation
   (format nil "string expression translate to standard expression ~A"
	   "(may have some S-expression in this)")))

(defmethod str-expr->stand-expr (string-expression)
  (str-expr->stand-expr (find-expr-class (string-upcase string-expression))))

(defclass normal-operator-expr ()
  ((number :initarg :number)
   (operator :initarg :operator)
   (next-expr :initarg :next-expr)))

(defmethod str-expr->stand-expr ((str-expr normal-operator-expr))
  (format t "num is: ~A; oper is: ~A; next is: ~A~%"
	  (slot-value str-expr 'number)
	  (slot-value str-expr 'operator)
	  (slot-value str-expr 'next-expr))
  (read)
  (append (str-expr->stand-expr
	   (find-expr-class (slot-value str-expr 'number)))
	  (find-operator str-expr)
	  (str-expr->stand-expr
	   (find-expr-class (slot-value str-expr 'next-expr)))))

(defun find-number (string-expression)
  (let ((val (slot-value string-expression 'number)))
    (if val
      (list (read-from-string val)))))

(defun find-operator (string-expression)
  (let ((val (slot-value string-expression 'operator)))
    (if val (list (intern val)))))

(anve-defun find-expr-class
    (multiple-value-bind (direction num oper next) (search-operator string)
      (cond))
    string)

(anve-defbody find-expr-class (str) 'first
  t (make-instance 'normal-operator-expr :number num :operator oper
		   :next-expr next))
(defclass null-operator-expr () ((value :initform nil)))

(anve-defbody find-expr-class (str) 'first
  (or (string-equal str "") (null str)) (make-instance 'null-operator-expr))
(defmethod str-expr->stand-expr ((str-expr null-operator-expr)) nil)

(defclass only-number-expr () ((number :initarg :number)))
(defmethod str-expr->stand-expr ((str-expr only-number-expr))
  (list (read-from-string (slot-value str-expr 'number))))
(anve-defbody find-expr-class (str) 'first
  (and num (null oper) (null next))
  (make-instance 'only-number-expr :number num))

(anve-update-function find-expr-class)
