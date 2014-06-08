#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass layer ()
  ((%pixmap :accessor pixmap)
   (%painter :accessor painter)
   (%stroke :initform () :accessor stroke)
   (%mode :initarg :mode :accessor mode)
   (%name :initarg :name :accessor name)))

(defmethod print-object ((layer layer) stream)
  (print-unreadable-object (layer stream :type T :identity T)
    (format stream "~a" (name layer)))
  layer)

(defmethod initialize-instance :after ((layer layer) &key)
  (let* ((pixmap (#_new QImage 500 500 (#_QImage::Format_ARGB32_Premultiplied)))
         (painter (#_new QPainter pixmap))
         (transparent (#_new QColor 0 0 0 0)))
    (#_fill pixmap transparent)
    (#_setRenderHint painter (#_QPainter::Antialiasing))
    (#_setRenderHint painter (#_QPainter::HighQualityAntialiasing))
    (#_setStyle (#_brush painter) (#_Qt::SolidPattern))
    (setf (pixmap layer) pixmap
          (painter layer) painter)))

(defmethod start-stroke ((layer layer) type x y x-tilt y-tilt pressure)
  (setf (stroke layer) (make-instance 'stroke))
  (record-point layer x y x-tilt y-tilt pressure)
  layer)

(defmethod record-point ((layer layer) x y x-tilt y-tilt pressure)
  (record-point (stroke layer) x y x-tilt y-tilt pressure)
  (draw-incremental (stroke layer) (painter layer))
  layer)

(defmethod draw ((layer layer) painter)
  (#_drawImage painter 0 0 (pixmap layer)))

(defmethod finalize ((layer layer))
  (optimized-delete (painter layer))
  (optimized-delete (pixmap layer))
  (finalize (stroke layer)))
