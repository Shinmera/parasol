#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass brush-class (standard-class)
  ((direct-fields :initform () :initarg :fields :accessor class-direct-fields)
   (fields :initform () :accessor class-fields)))

(defmethod c2mop:validate-superclass ((class brush-class) (superclass t))
  nil)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass brush-class))
  nil)

(defmethod c2mop:validate-superclass ((class brush-class) (superclass standard-class))
  t)

(defmethod c2mop:validate-superclass ((class brush-class) (superclass brush-class))
  t)

(defun compute-fields (class &key (direct-superclasses (c2mop:class-direct-superclasses class))
                               (fields (class-direct-fields class)))
  (append (loop with superfields
                for superclass in direct-superclasses
                do (loop for field in (when (typep superclass 'brush-class)
                                        (class-fields superclass))
                         unless (or (find (car field) superfields :key #'car)
                                    (find (car field) fields :key #'car))
                           do (push field superfields))
                finally (return superfields))
          (remove :remove fields :test #'find)))

(defun cascade-field-changes (class)
  (setf (class-fields class) (compute-fields class))
  (loop for sub-class in (c2mop:class-direct-subclasses class)
        when (and (typep sub-class 'brush-class)
                  (c2mop:class-finalized-p sub-class))
          do (cascade-field-changes sub-class)))

(defun initialize-brush-class (class next-method &rest args &key direct-superclasses fields &allow-other-keys)
  (let ((computed-fields (compute-fields class :fields fields :direct-superclasses direct-superclasses)))
    (setf (class-fields class) computed-fields)
    (apply next-method
           class
           :allow-other-keys t
           :fields fields
           args)))

(defmethod initialize-instance :around ((class brush-class) &rest args)
  (apply #'initialize-brush-class class #'call-next-method args))

(defmethod reinitialize-instance :around ((class brush-class) &rest args)
  (apply #'initialize-brush-class class #'call-next-method args))

(defmethod c2mop:finalize-inheritance :after ((class brush-class))
  (dolist (super (c2mop:class-direct-superclasses class))
    (unless (c2mop:class-finalized-p super)
      (c2mop:finalize-inheritance super)))
  (cascade-field-changes class))

(defclass abstract-brush ()
  ((%name :initform "Abstract Brush" :accessor name)
   (%base-color :initarg :base-color :initform NIL :accessor base-color)
   (%point-distance :initarg :point-distance :initform 2 :accessor point-distance))
  (:metaclass brush-class)
  (:fields (point-distance :type :float :range (0.1 200.0 0.5) :slot %point-distance)))

(defclass brush (abstract-brush)
  ((%name :initform "Unnamed Brush" :accessor name))
  (:metaclass brush-class))

(defmethod draw-curve ((brush abstract-brush) painter curve from to)
  (#_setColor (#_brush painter) (base-color brush))
  (#_setColor (#_pen painter) (base-color brush))
  (map-points curve #'(lambda (x y xt yt p) (draw-point brush painter x y xt yt p))
              :from from :to to))

(defmethod draw-point ((brush abstract-brush) painter x y xt yt p)
  (declare (ignore xt yt))
  (let ((len (* p 10)))
    (with-objects ((point (#_new QPointF x y)))
      (#_drawEllipse painter point len len))))

(defun slot-initarg (class slot)
  (first (c2mop:slot-definition-initargs
          (find slot (c2mop:class-slots class)
                :key #'c2mop:slot-definition-name))))

(defmethod assume-form ((brush abstract-brush))
  (apply #'make-instance
         (class-name (class-of brush))
         :base-color (#_new QColor (color *window*))
         (loop with args
               for field in (class-fields (class-of brush))
               do (destructuring-bind (name &key (slot name) &allow-other-keys) field
                    (push (slot-value brush slot) args)
                    (push (slot-initarg (class-of brush) slot) args))
               finally (return args))))

(defmethod finalize ((brush abstract-brush))
  (maybe-delete-qobject (base-color brush)))

(defun error-on-not-found (list allowed)
  (dolist (opt list)
    (unless (member opt allowed)
      (error "~a is not a valid option." opt))))

(defgeneric build-brush-element (type name &key range &allow-other-keys))

(defgeneric brush-ui (brush)
  (:method ((brush abstract-brush))
    (loop for field in (class-fields (class-of brush))
          collect (apply #'build-brush-element (getf (cdr field) :type) (car field) (cdr field)))))

(defmacro define-brush (name direct-superclasses direct-slots &body options)
  (destructuring-bind (class-name &optional
                                    (name (string-downcase class-name))
                                    (package (make-symbol (format NIL "ORG.TYMOONNEXT.PARASOL.BRUSH.~a" class-name)))) (if (listp name) name (list name))
    (let ((fields (cdr (assoc :fields options)))
          (documentation (cdr (assoc :documentation options)))
          (finalize (cdr (assoc :finalize options)))
          (draw (cdr (assoc :draw options)))
          (draw-point (cdr (assoc :draw-point options))))
      (error-on-not-found (mapcar #'car options) '(:fields :slots :superclasses :documentation :finalize :draw :draw-point))
      (dolist (field fields)
        (destructuring-bind (name &key type range &allow-other-keys) field
          (unless type (error "Field ~a has no type definition." name))
          (ecase type (:integer) (:float) (:file) (:string) (:boolean))
          (if range
              (unless (or (eq type :integer) (eq type :float))
                (error "Field ~a is of type ~a and thus cannot have a range definition." name type))
              (when (or (eq type :integer) (eq type :float))
                (error "Field ~a is of type ~a and thus requires a range definition." name type)))))
      `(progn
         ,@(when package
             `((defpackage ,package
                 (:use #:cl #:qt)
                 (:import-from #:parasol #:define-brush)
                 (:export ,(make-symbol (string class-name))))
               (in-package ,package)
               (named-readtables:in-readtable :qt)))
         (defclass ,class-name (,@direct-superclasses)
           ((%name :initform ,name :accessor name)
            ,@direct-slots)
           (:metaclass brush-class)
           (:fields ,@fields)
           ,@(when documentation
               `((:documentation ,documentation))))
         ,@(when draw
             `((defmethod draw-curve ((,(caar draw) ,class-name) ,@(cdar draw))
                 ,@(cdr draw))))
         ,@(when draw-point
             `((defmethod draw-point ((,(caar draw-point) ,class-name) ,@(cdar draw-point))
                 ,@(cdr draw-point))))
         ,@(when finalize
             `((defmethod finalize ((,(caar finalize) ,class-name))
                 ,@(cdr finalize))))))))
