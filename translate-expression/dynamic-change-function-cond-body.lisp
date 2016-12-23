(defpackage :use-for-save-anve-generic-body-package)

(set-macro-character #\} (get-macro-character #\)))
(set-macro-character #\{
  #'(lambda (stream &rest body)
      (setf body 0)
      (cons 'progn (read-delimited-list #\} stream t))))

(defun last-pair (list)
  (if (null (cdr list))
      list
      (last-pair (cdr list))))

(defmacro set-last (list value)
  `{(setf (cdr (last-pair ,list)) ,value) ,list})

(defun make-package-name (name)
  (let ((symbol-name (if (stringp name) (string-upcase name) (string name))))
    (intern symbol-name :use-for-save-anve-generic-body-package)))

(defun anve-change-body (args new-args body)
  (if (null args)
      body
      (anve-change-body (cdr args) (cdr new-args)
			(let ((a (car args)) (b (car new-args)))
			  (mapcar #'(lambda (x)
				      (cond ((consp x)
					     (anve-change-body args new-args x))
					    ((eql a x) b)
					    (t x)))
				  body)))))

(defun make-initial-args (list)
  (mapcar (let ((x 0))
	    #'(lambda (y) y ;just for no warning
		(incf x)
		(make-package-name (format nil "ARG-~A" x))))
	  list))

(defmacro anve-defun (name action &body lambda-list)
  (let ((body-list (gensym))
	(args-list (gensym))
	(action-nm (gensym))
	(func-name (make-package-name name)))
    `{
       (let ((,body-list nil)
	     (,args-list ',lambda-list)
	     (,action-nm ',action))
	 (defun ,func-name (&optional (how-do? 'get-body) value)
	   (declare (type list ,body-list ,args-list value)
		    (type symbol ,action-nm how-do?))
	   (cond ((string-equal how-do? 'get-body) ,body-list)
		 ((string-equal how-do? 'get-args) ,args-list)
		 ((string-equal how-do? 'get-action) ,action-nm)
		 ((and value (not (listp value)))
		  (error "The ~A is not of type LIST" value))
		 ((and (string-equal how-do? 'append-body) value)
		  (setq ,body-list (nconc value ,body-list)))
		 ((and (string-equal how-do? 'push-body)value)
		  (push value ,body-list))
		 ((string-equal how-do? 'pop-body) (pop ,body-list))
		 ((and (string-equal how-do? 'set-body) value)
		  (setq ,body-list value))
		 ((and (string-equal how-do? 'set-body-last) value)
		  (set-last ,body-list (list value)))
		 ((or (string-equal how-do? 'set-body)
		      (string-equal how-do? 'append-body)
		      (string-equal how-do? 'push-body))
		  (error "You musi input a VALUE in ~A" ',func-name))
		 (t (error "Unknown type in ~A: ~A" ',func-name how-do?)))))
       (defun ,name ,lambda-list (list ,@lambda-list))}))

(defmacro anve-update-function (name)
  (let* ((name-of-package (make-package-name name))
	 (lambda-list (funcall name-of-package 'get-args))
	 (body-arg-list (make-initial-args lambda-list)))
    `{  (setf (symbol-function ',name)
	      #'(lambda ,lambda-list
		  ,(anve-change-body body-arg-list lambda-list
				     (cons (funcall name-of-package 'get-action)
					   (funcall name-of-package)))))
        ',name}))



(defmacro anve-defbody (name lambda-list place &body body)
  (let ((func-name (make-package-name name))
	(true-body `(anve-change-body ',lambda-list
				      (make-initial-args ',lambda-list)
				      ',body)))
    `(cond ((not (= (length ',lambda-list) (length (,func-name 'get-args))))
	    (error "In lambda-list"))
	   ((eq ,place 'first)
	    (,func-name 'push-body ,true-body))
	   ((eq ,place 'last)
	    (,func-name 'set-body-last ,true-body))
	   (t (error "Unknown place in ~A" ',name)))))
					;(anve-update-function)}))

