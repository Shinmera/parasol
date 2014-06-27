#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass raster-item ()
  ((%width :initform 0 :initarg :width :accessor width)
   (%height :initform 0 :initarg :height :accessor height)
   (%offset-x :initform 0 :initarg :offset-x :accessor offset-x)
   (%offset-y :initform 0 :initarg :offset-y :accessor offset-y)

   (%pixmap :initform NIL :initarg :pixmap :accessor pixmap)
   (%painter :initform NIL :initarg :painter :accessor painter)))

(defmethod initialize-instance :after ((item raster-item) &key)
  (when (and (not (width item))
             (not (height item))
             (pixmap item))
    (setf (width item) (#_width (pixmap item))
          (height item) (#_height (pixmap item)))))

(defmethod draw ((item raster-item) painter)
  (when (pixmap item)
    (#_drawImage painter (offset-x item) (offset-y item) (pixmap item))))

(defmethod finalize ((item raster-item))
  (when (painter item) (#_end (painter item)))
  (cleanup (item) painter pixmap))
