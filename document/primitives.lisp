#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(named-readtables:in-readtable :qtools)

(defclass drawable ()
  ())

(defgeneric draw (drawable target))

(defclass positioned ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))

(defgeneric translate-to (positioned transform)
  (:method ((pos positioned) (transform qobject))
    (#_translate transform (x pos) (y pos))))

(defgeneric translate-away (positioned transform)
  (:method ((pos positioned) (transform qobject))
    #_translate transfrom (- (x pos)) (- (y pos))))

(defmacro with-translation ((positioned transform) &body body)
  (let ((pos (gensym "POSITIONED"))
        (tr (gensym "TRANSFORM")))
    `(let ((,pos ,positioned) (,tr ,transform))
       (translate-to ,pos ,tr)
       (unwind-protect
            (progn ,@body)
         (translate-away ,pos ,tr)))))

(defclass buffered ()
  ((buffer :initarg :buffer :accessor buffer)))

(defmethod initialize-instance :after ((buffer buffered) &key)
  (unless buffer
    (error "Buffer not initialized!")))

(defgeneric rebuffer (buffered)
  (:method ((buffered buffered))
    (with-finalizing ((painter (#_new QPainter (buffer buffered))))
      (#_eraseRect painter 0 0 (#_width (buffer buffered)) (#_height (buffer buffered)))
      (draw buffered painter))))

(defgeneric rebuffer-copy (buffered)
  (:method ((buffered buffered))
    (let* ((old (buffer buffered))
           (new (#_new QImage (#_size old) (#_format old))))
      (with-finalizing ((painter (#_new QPainter new)))
        (draw buffered painter))
      (setf (buffer buffered) new)
      (finalize old))))