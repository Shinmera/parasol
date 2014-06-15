#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defvar *graphics* (merge-pathnames "graphics/" (asdf:system-source-directory :parasol)))

;; Curve helper stuff
(defun idata (var data-slot pos)
  (aref (aref var data-slot) pos))

(defgeneric (setf idata) (val var data-slot pos)
  (:method (val var data-slot pos)
    (setf (aref (aref var data-slot) pos) val)))

(defun copy-adjustable (array)
  (let ((new (make-array (length array) :element-type 'float :adjustable T :fill-pointer T)))
    (loop for i from 0 below (length array)
          do (setf (aref new i) (aref array i)))
    new))

(defun ensure-length (vector required-length)
  (cond ((< (array-dimension vector 0) required-length)
         (adjust-array vector (+ required-length *spline-adjust-buffer*)
                       :fill-pointer required-length))
        ((< (fill-pointer vector) required-length)
         (setf (fill-pointer vector) required-length)))
  vector)

;; Qt helper stuff
(defgeneric copy-qobject (qclass instance)
  (:method :before (qclass instance)
    (format T "~& Copying QObject: ~a~%" instance))
  ;; QImage
  (:method ((qclass (eql 11848)) instance) 
    (#_copy instance))
  ;; QColor
  (:method ((qclass (eql 3976)) instance)
    (#_new QColor instance)))

(defmacro qtenumcase (keyform &body forms)
  (let ((key (gensym "KEY")))
    `(let ((,key ,keyform))
       (cond ,@(loop for form in forms
                     collect `((qt:enum= ,key ,(car form)) ,@(cdr form)))))))

(defun qobject-alive-p (object)
  (not (or (null-qobject-p object)
           (qobject-deleted object))))

(defun maybe-delete-qobject (object)
  (if (typep object 'abstract-qobject)
      (when (qobject-alive-p object)
        (format T "~& Deleting QObject: ~a~%" object)
        (optimized-delete object))
      (format T "~& Deleting QObject: WARN Tried to delete non-qobject ~a~%" object)))

(defun finalize-and-delete (object)
  (finalize object)
  (when (typep object 'abstract-qobject)
    (maybe-delete-qobject object))
  object)

(defmacro cleanup ((instance-form) &rest accessors)
  (let ((instance (gensym "INSTANCE")))
    `(let ((,instance ,instance-form))
       ,@(loop for accessor in accessors
               collect `(finalize-and-delete (,accessor ,instance)))
       (setf ,@(loop with stuff = ()
                     for accessor in accessors
                     do (push NIL stuff)
                        (push `(,accessor ,instance) stuff)
                     finally (return stuff))))))

(defmacro with-dialog ((var instance-form) &body setup-forms)
  `(with-objects ((,var ,instance-form))
     ,@setup-forms
     (#_exec ,var)
     (finalize ,var)))

(defmacro with-transform ((painter-form) &body body)
  (let ((painter (gensym "PAINTER"))
        (transform (gensym "TRANSFORM")))
    `(let ((,painter ,painter-form))
       (let ((,transform (#_new QTransform (#_worldTransform ,painter))))
         ,@body
         (#_setWorldTransform ,painter ,transform)))))
