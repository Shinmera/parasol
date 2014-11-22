#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.document)
(named-readtables:in-readtable :qtools)

(define-finalizable positioned ()
  ((x :initarg :x :initform 0.0 :accessor x)
   (y :initarg :y :initform 0.0 :accessor y)))

(defgeneric translate-to (positioned transform)
  (:method ((pos positioned) (transform qobject))
    (#_translate transform (#_new QPointF (x pos) (y pos)))))

(defgeneric translate-away (positioned transform)
  (:method ((pos positioned) (transform qobject))
    (#_translate transform (#_new QPointF (- (x pos)) (- (y pos))))))

(defmacro with-transformation ((painter) &body body)
  (let ((paint (gensym "PAINTER")))
    `(let* ((,paint ,painter))
       (unwind-protect
            (progn
              (#_save ,paint)
              ,@body)
         (#_restore ,paint)))))

(defmacro with-translation-to ((positioned transform) &body body)
  (let ((pos (gensym "POSITIONED"))
        (tr (gensym "TRANSFORM")))
    `(let ((,pos ,positioned) (,tr ,transform))
       (translate-to ,pos ,tr)
       (unwind-protect
            (progn ,@body)
         (translate-away ,pos ,tr)))))

(defmacro with-translation-away ((positioned transform) &body body)
  (let ((pos (gensym "POSITIONED"))
        (tr (gensym "TRANSFORM")))
    `(let ((,pos ,positioned) (,tr ,transform))
       (translate-away ,pos ,tr)
       (unwind-protect
            (progn ,@body)
         (translate-to ,pos ,tr)))))

(define-finalizable drawable (positioned)
  ())

(defgeneric draw (drawable target)
  (:method :around ((drawable drawable) target)
    (with-translation-to (drawable target)
      (call-next-method))))

(define-finalizable buffered (drawable)
  ((buffer :initarg :buffer :initform NIL :accessor buffer)
   (painter :initarg :painter :initform NIL :accessor painter)))

(defmethod finalize :before ((buffered buffered))
  ;; The order is important, so we won't rely on the finalized slot attribute.
  (finalize (painter buffered))
  (finalize (buffer buffered))
  buffered)

(defmethod (setf buffer) :around (buffer (buffered buffered))
  (unless (eql buffer (buffer buffered))
    (when (painter buffered)
      (finalize (painter buffered)))
    (when (buffer buffered)
      (finalize (buffer buffered)))
    (call-next-method)
    (setf (painter buffered) (make-painter buffer))))

(defgeneric draw-buffer (buffered target)
  (:method :around ((buffered buffered) target)
    (with-translation-away (buffered target)
      (call-next-method))))

(defgeneric rebuffer (buffered)
  (:method ((buffered buffered))
    (let ((painter (painter buffered)))
      (#_eraseRect painter 0 0 (#_width (buffer buffered)) (#_height (buffer buffered)))
      (draw-buffer buffered painter))))

(defgeneric rebuffer-copy (buffered)
  (:method ((buffered buffered))
    (let* ((old (buffer buffered))
           (new (make-target (width old) (height old))))
      (with-finalizing ((painter (#_new QPainter new)))
        (draw-buffer buffered painter))
      (setf (buffer buffered) new))))

(defmethod draw ((buffered buffered) target)
  (when (buffer buffered)
    (draw (buffer buffered) target)))

(define-finalizable adaptive-buffered (buffered positioned)
  ((chunk-size :initarg :chunk-size :initform 1000 :accessor chunk-size)
   (initial-size :initarg :initial-size :initform 500 :accessor initial-size)))

(defgeneric ensure-fitting (x y layer)
  (:method (x y (buffered adaptive-buffered))
    (unless (buffer buffered)
      (setf (x buffered) (- x (/ (initial-size buffered) 2))
            (y buffered) (- y (/ (initial-size buffered) 2)))    
      (setf (buffer buffered) (make-target (initial-size buffered)
                                           (initial-size buffered))))
    (multiple-value-bind (target xd yd) (ensure-containable
                                         (- x (x buffered)) (- y (y buffered)) (buffer buffered)
                                         :chunk-size (chunk-size buffered))
      (setf (buffer buffered) target)
      (incf (x buffered) xd)
      (incf (y buffered) yd))
    buffered))
