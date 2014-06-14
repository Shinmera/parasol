#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass abstract-brush ()
  ((%name :initform "Abstract Brush" :accessor name)
   (%base-size :initarg :base-size :initform 5 :accessor base-size)
   (%base-color :initarg :base-color :initform NIL :accessor base-color)
   (%point-distance :initarg :point-distance :initform 2 :accessor point-distance)))

(defclass brush (abstract-brush)
  ((%name :initform "Abstract Brush" :accessor name)))

(defmethod assume-form ((brush abstract-brush))
  (make-instance (class-name (class-of brush))
                 :base-size (base-size brush)
                 :base-color (#_new QColor (color *window*))
                 :point-distance (point-distance brush)))

(defmethod draw-curve ((brush abstract-brush) painter curve from to)
  (#_setColor (#_brush painter) (base-color brush))
  (#_setColor (#_pen painter) (base-color brush))
  (map-points curve #'(lambda (x y xt yt p) (draw-point brush painter x y xt yt p))
              :from from :to to))

(defmethod draw-point ((brush abstract-brush) painter x y xt yt p)
  (declare (ignore xt yt))
  (let ((len (* p (base-size brush))))
    (with-objects ((point (#_new QPointF x y)))
      (#_drawEllipse painter point len len))))

(defmethod finalize ((brush abstract-brush))
  (maybe-delete-qobject (base-color brush)))

(defun error-on-not-found (list allowed)
  (dolist (opt list)
    (unless (member opt allowed)
      (error "~a is not a valid option." opt))))

(defun build-brush-field (name &key default type range)
  )

(defmacro define-brush (name &body options)
  (destructuring-bind (class-name &optional
                                    (name (string-downcase class-name))
                                    (package (make-symbol (format NIL "ORG.TYMOONNEXT.PARASOL.BRUSH.~a" class-name)))) (if (listp name) name (list name))
    (let ((fields (cdr (assoc :fields options)))
          (field-data ())
          (slots (cdr (assoc :slots options)))
          (superclasses (cdr (assoc :superclasses options)))
          (documentation (cdr (assoc :documentation options)))
          (finalize (cdr (assoc :finalize options)))
          (draw (cdr (assoc :draw options)))
          (draw-point (cdr (assoc :draw-point options))))
      (error-on-not-found (mapcar #'car options) '(:fields :slots :superclasses :documentation :finalize :draw :draw-point))
      (dolist (field fields)
        (destructuring-bind (name &key default type range) field
          (unless type (error "Field ~a has no type definition." name))
          (ecase type (:integer) (:float) (:file) (:string) (:boolean))
          (if range
              (unless (or (eq type :integer) (eq type :float))
                (error "Field ~a is of type ~a and thus cannot have a range definition." name type))
              (when (or (eq type :integer) (eq type :float))
                (error "Field ~a is of type ~a and thus requires a range definition." name type)))
          
          (push `(,name :initform ,default :accessor ,name) slots)
          (push `(build-brush-field :name ',name :type type :default default :range ',range) field-data)))
      `(progn
         ,@(when package
             `((defpackage ,package
                 (:use #:cl #:qt)
                 (:import-from #:parasol #:define-brush)
                 (:export ,(make-symbol (string class-name))))
               (in-package ,package)
               (named-readtables:in-readtable :qt)))
         (defclass ,class-name (,@superclasses brush)
           ((%name :initform ,name :accessor name)
            ,@slots)
           ,@(when documentation
               `((:documentation ,documentation))))
         (defmethod build-ui ((,(gensym "BRUSH") ,class-name))
           (list ,@field-data))
         ,@(when draw
             `((defmethod draw-curve ((,(caar draw) ,class-name) ,@(cdar draw))
                 ,@(cdr draw))))
         ,@(when draw-point
             `((defmethod draw-point ((,(caar draw-point) ,class-name) ,@(cdar draw-point))
                 ,@(cdr draw-point))))
         ,@(when finalize
             `((defmethod finalize ((,(caar finalize) ,class-name))
                 ,@(cdr finalize))))))))
