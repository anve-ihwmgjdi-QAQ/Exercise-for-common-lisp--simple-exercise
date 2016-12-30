(defpackage :use-for-save-anve-symbol-body-package.-)
;;(defpackage :string-translate.str-tran (:use :common-lisp))

;(in-package :use-for-save-anve-symbol-body-package.-)
;(cl::defvar *anve-function-list-save-place* (cl::make-hash-table))
;(cl::defvar *operator-save-place* (cl::make-hash-table))
;(in-package :str-translate.str-tran)

(defun make-package-name (name)
  (let ((symbol-name (if (stringp name) (string-upcase name) (string name))))
    (intern symbol-name :use-for-save-anve-symbol-body-package.-)))

(defvar *anve-function-list-save-place* (make-hash-table))
(defvar *operator-list-save-place* (make-hash-table))
