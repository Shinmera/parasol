#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass texture-brush (abstract-brush)
  ((%name :initform "Texture Brush" :accessor name)
   (%point-distance :initarg :point-distance :initform 32 :accessor point-distance)
   (%texture-scale :initarg :texture-scale :initform 1.0 :accessor texture-scale)
   (%texture :initarg :texture :initform (error "Texture Required") :accessor texture))
  (:metaclass brush-class)
  (:fields (texture-scale :type :float :range (0.01 2.00) :slot %texture-scale)))

(defmethod draw-point ((brush texture-brush) painter x y xt yt p)
  (declare (ignore xt yt))
  (let ((transform (#_new QTransform (#_worldTransform painter)))
        (p (* p (texture-scale brush))))
    (with-objects ((point (#_new QPointF
                                 (- x (* (#_width (texture brush)) p 1/2))
                                 (- y (* (#_height (texture brush)) p 1/2)))))
      (#_translate painter point)
      (#_scale painter p p)
      (#_drawImage painter 0 0 (texture brush)))
    (#_setWorldTransform painter transform)))

(defmethod finalize ((brush texture-brush))
  (maybe-delete-qobject (texture brush))
  (call-next-method))

(defclass pepper-brush (texture-brush brush)
  ((%name :initform "Pepper Brush" :accessor name)
   (%texture :initarg :texture :initform (#_new QImage (uiop:native-namestring (merge-pathnames "pepper.png" *graphics*))) :accessor texture))
  (:metaclass brush-class))

(defclass jalapeno-brush (texture-brush brush)
  ((%name :initform "Jalapeno Brush" :accessor name)
   (%texture :initarg :texture :initform (#_new QImage (uiop:native-namestring (merge-pathnames "jalapeno.png" *graphics*))) :accessor texture))
  (:metaclass brush-class))
