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

(defmethod draw-curve ((brush abstract-brush) painter curve &key from to)  
  (unless from (setf from 0))
  (unless to (setf to (point-amount curve)))
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
