#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defvar *layer-block-size* 500)

(defclass layer ()
  ((%width :initform 0 :accessor width)
   (%height :initform 0 :accessor height)
   (%offset-x :initform 0 :accessor offset-x)
   (%offset-y :initform 0 :accessor offset-y)

   (%pixmap :initform NIL :accessor pixmap)
   (%painter :initform NIL :accessor painter)
   (%stroke :initform () :accessor stroke)
   (%mode :initarg :mode :accessor mode)
   (%name :initarg :name :accessor name)))

(defmethod print-object ((layer layer) stream)
  (print-unreadable-object (layer stream :type T :identity T)
    (format stream "~a" (name layer)))
  layer)

(defmethod initialize-instance :after ((layer layer) &key)
  (assure-suitable-size layer 1 1))

(defmethod assure-suitable-size ((layer layer) x y)
  (unless (and (< (offset-x layer) x (+ (offset-x layer) (width layer)))
               (< (offset-y layer) y (+ (offset-y layer) (height layer))))
    (let ((width (* *layer-block-size* (ceiling (/ x *layer-block-size*))))
          (height (* *layer-block-size* (ceiling (/ y *layer-block-size*))))
          (xoff (offset-x layer))
          (yoff (offset-y layer)))
      (when (< width 0)
        (incf xoff width)
        (incf width (width layer)))
      (when (< height 0)
        (incf yoff height)
        (incf height (height layer)))
      (setf width (max width (width layer)))
      (setf height (max height (height layer)))
      (format T "~a ASSURING NEW BOUNDS: ~a,~a ~a/~a~%" layer xoff yoff width height)
      (let* ((pixmap (#_new QImage width height (#_QImage::Format_ARGB32_Premultiplied)))
             (painter (#_new QPainter pixmap))
             (transparent (#_new QColor 0 0 0 0)))
        (#_fill pixmap transparent)
        (when (pixmap layer)
          (#_drawImage painter 0 0 (pixmap layer))
          (optimized-delete (pixmap layer)))
        (#_setRenderHint painter (#_QPainter::Antialiasing))
        (#_setRenderHint painter (#_QPainter::HighQualityAntialiasing))
        (#_setStyle (#_brush painter) (#_Qt::SolidPattern))
        (setf (pixmap layer) pixmap
              (painter layer) painter
              (offset-x layer) xoff
              (offset-y layer) yoff
              (width layer) width
              (height layer) height)))))

(defmethod start-stroke ((layer layer) type x y x-tilt y-tilt pressure)
  (setf (stroke layer) (make-instance 'stroke))
  (record-point layer x y x-tilt y-tilt pressure)
  layer)

(defmethod record-point ((layer layer) x y x-tilt y-tilt pressure)
  (assure-suitable-size layer x y)
  (record-point (stroke layer) x y x-tilt y-tilt pressure)
  (draw-incremental (stroke layer) (painter layer))
  layer)

(defmethod draw ((layer layer) painter)
  (#_drawImage painter (offset-x layer) (offset-y layer) (pixmap layer)))

(defmethod finalize ((layer layer))
  (optimized-delete (painter layer))
  (optimized-delete (pixmap layer))
  (finalize (stroke layer)))
