(load "dynamic-change-function-cond-body.lisp")

;(defvar *operator-list-save-place* (make-hash-table :test #'eq))


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

(make-operate-list operate-parenthesis-lst  (#\( #\) #\[ #\] #\{ #\}))
(make-operate-list operate-normal-oper-lst (+ - * /))
;;; special operator is like "3sin4" hide a * in the expressions
(make-operate-list operate-special-oper-lst (sin cos tan log))

(defvar *use-for-search-other-half-parenthesis* (make-hash-table :test 'equalp))
(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n) acc))
		   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))
(defmacro set-parenthesis-pair-hash-table (&rest args)
  `(progn
     ,@(mapcar #'(lambda (pair)
		   `(setf (gethash ,(car pair)
				   *use-for-search-other-half-parenthesis*)
			  ,@(cdr pair)))
	       (group args 2))))
(set-parenthesis-pair-hash-table "(" ")" "[" "]" "{" "}")


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

(anve-defun find-expr-class
    (multiple-value-bind ([direction] [num] [oper] [next]) (search-operator string)
      (cond))
    string)

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
	  (list (intern (slot-value str-expr 'operator)))
	  (str-expr->stand-expr
	   (find-expr-class (slot-value str-expr 'next-expr)))))

(anve-defbody find-expr-class (str) 'first
  t (make-instance 'normal-operator-expr :number [num] :operator [oper]
		   :next-expr [next]))

(defclass parentheses-expr ()
  ((before-paren :initarg :before-paren)
   (in-paren :initarg :in-paren)
   (after-paren :initarg :after-paren)))

(defun last-parenssion (string)
  (labels ((search-paren (type start &optional end)
	     (maphash #'(lambda (open-paren clo-paren)
			  (if (eq type 'close)
			      (return-from search-paren
				(let ((ser (search clo-paren string
						   :test #'string-equal
						   :start2 start :end2 end)))
				  (values ser
					  (if ser (+ (length clo-paren) ser)))))
			      (return-from search-paren
				(let ((ser (search open-paren string
						   :test #'string-equal
						   :start2 start :end2 end)))
				  (values ser
					  (if ser (length clo-paren)))))))
		      *use-for-search-other-half-parenthesis*))
	   (iter (start-n end-n)
	     (multiple-value-bind (end-s0 en-len) (search-paren 'close end-n)
	       (multiple-value-bind (start-s0 st-len) (search-paren 'open start-n
								    end-s0)
		 (if start-s0
		     (iter (+ start-s0 st-len) (+ end-s0 st-len))
		     (values end-s0 en-len))))))
    (iter 0 0)))



(defmethod str-expr->stand-expr ((str-expr parentheses-expr))
  (append (str-expr->stand-expr
	   (find-expr-class (slot-value str-expr 'before-paren)))
	  (list (str-expr->stand-expr
		 (find-expr-class (slot-value str-expr 'in-paren))))
	  (str-expr->stand-expr
	   (find-expr-class (slot-value str-expr 'after-paren))))))
(anve-defbody find-expr-class (str) 'first
  (eq [direction] (make-package-name 'operate-parenthesis-lst))
  (multiple-value-bind (start-paren end-paren) (last-parenssion [next])
    (format t "num is: ~A; oper is: ~a; next is: ~A; start is: ~a end is:~A~%"
	    [num] [oper] [next] start-paren end-paren)
    (make-instance 'parentheses-expr :before-paren [num]
		   :in-paren (subseq [next] 0 start-paren)
		   :after-paren (subseq [next] end-paren))))

;(defun find-number (string-expression)
 ; (let ((val (slot-value string-expression 'number)))
  ;  (if val
   ;   (list (read-from-string val)))))

;(defun find-operator (string-expression)
 ; (let ((val (slot-value string-expression 'operator)))
  ;  (if val (list (intern val)))))



(defclass only-number-expr () ((number :initarg :number)))
(defmethod str-expr->stand-expr ((str-expr only-number-expr))
  (list (read-from-string (slot-value str-expr 'number))))
(anve-defbody find-expr-class (str) 'first
  (and [num] (null [oper]) (null [next]))
  (make-instance 'only-number-expr :number [num]))

(defclass null-operator-expr () ((value :initform nil)))
(anve-defbody find-expr-class (str) 'first
  (or (string-equal str "") (null str)) (make-instance 'null-operator-expr))
(defmethod str-expr->stand-expr ((str-expr null-operator-expr)) nil)

(anve-update-function find-expr-class)
