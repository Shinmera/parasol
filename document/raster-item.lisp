#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass raster-item ()
  ((%width :initform 0 :accessor width)
   (%height :initform 0 :accessor height)
   (%offset-x :initform 0 :accessor offset-x)
   (%offset-y :initform 0 :accessor offset-y)

   (%pixmap :initform NIL :accessor pixmap)
   (%painter :initform NIL :accessor painter)))

(defmethod draw ((raster raster-item) painter)
  (when (pixmap raster)
    (#_drawImage painter (offset-x raster) (offset-y raster) (pixmap raster))))

(defmethod finalize ((raster raster-item))
  (when (painter raster) (#_end (painter raster)))
  (cleanup (raster) painter pixmap))
