#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defmacro qtenumcase (keyform &body forms)
  (let ((key (gensym "KEY")))
    `(let ((,key ,keyform))
       (cond ,@(loop for form in forms
                     collect `((qt:enum= ,key ,(car form)) ,@(cdr form)))))))

(defun maybe-delete-qobject (object)
  (if object
      (unless (or (typep object 'null-qobject)
                  (qobject-deleted object))
        (format T "~& Deleting: ~a~%" object)
        (optimized-delete object))
      (format T "~& Deleting: WARN Tried to delete NIL~%")))
