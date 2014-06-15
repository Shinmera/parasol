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
  (let ((p (* p (texture-scale brush))))
    (with-objects ((point (#_new QPointF
                                 (* (#_width (texture brush)) -1/2)
                                 (* (#_height (texture brush)) -1/2))))
      (#_scale painter p p)
      (#_drawImage painter point (texture brush)))))

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

(defun set-brush-texture (slot path)
  (let ((curtex (slot-value (current-brush *window*) slot)))
    (when curtex (finalize-and-delete curtex)))
  (setf (slot-value (current-brush *window*) slot)
        (#_new QImage (uiop:native-namestring path))))

(defclass textured-brush (texture-brush brush)
  ((%name :initform "Textured Brush" :accessor name)
   (%texture :initarg :texture :initform NIL :accessor texture))
  (:metaclass brush-class)
  (:fields (texture :type :file :slot %texture :setter #'set-brush-texture
                    :filters "Image Files (*.png *.jpg *.jpeg *.bmp")))
