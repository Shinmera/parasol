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

;; Qt helper stuff
(defgeneric copy-qobject (qclass instance)
  (:method :before (qclass instance)
    (v:trace :cleanup "Copying QObject: ~a" instance))
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
        (v:trace :cleanup "Deleting QObject: ~a" object)
        (optimized-delete object))
      (v:trace :cleanup "Deleting QObject: WARN Tried to delete non-qobject ~a" object)))

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

(defmacro with-cleanup-on-error ((&rest cleanup-vars) (&rest error-types) &body body)
  `(handler-bind (((or ,@error-types)
                    #'(lambda (err)
                        (v:debug :cleanup "WITH-CLEANUP-ON-ERROR triggered due to ~a. Cleaning up ~a" err ',cleanup-vars)
                        ,@(loop for var in cleanup-vars collect `(finalize-and-delete ,var)))))
     ,@body))

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
         (unwind-protect
              (progn ,@body)
           (#_setWorldTransform ,painter ,transform))))))

(defmacro with-painter ((painter-var target-form) &body body)
  `(with-objects ((,painter-var (#_new QPainter ,target-form)))
     (unwind-protect
          (progn ,@body)
       (#_end ,painter-var))))

(defparameter *compositing-mode-list*
  '(("Normal" 0)
    ("Erase" 8)
    ("Lock Alpha" 9)
    ("Multiply" 13)
    ("Screen" 14)
    ("Overlay" 15)
    ("Darken" 16)
    ("Lighten" 17)
    ("Dodge" 18)
    ("Burn" 19)
    ("Hard Light" 20)
    ("Soft Light" 21)
    ("Difference" 22)
    ("Exclusion" 23)))

(defparameter *compositing-mode-map*
  (let ((map (make-hash-table :test 'equalp)))
    (loop for (key val) in *compositing-mode-list*
          do (setf (gethash key map) val))
    map))

(defun make-temporary-directory-name ()
  (format NIL "parasol-~10,'0d-~10,'0d" (get-universal-time) (random 10000000000)))

(defmacro with-temporary-directory ((pathname-value &key (base-dir (uiop:temporary-directory))
                                                      (sub-dir (make-temporary-directory-name))) &body body)
  `(let ((,pathname-value (merge-pathnames (uiop:ensure-directory-pathname ,sub-dir) ,base-dir)))
     (unwind-protect
          (progn
            (ensure-directories-exist ,pathname-value)
            ,@body)
       (uiop:delete-directory-tree ,pathname-value :validate (constantly T) :if-does-not-exist :ignore))))

(defun icon (name)
  (#_new QIcon (uiop:native-namestring (merge-pathnames (format NIL "~a.png" name) *graphics*))))
