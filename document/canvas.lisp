#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass canvas ()
  (;; Properties
   (%width :initform 500 :initarg :width :accessor width)
   (%height :initform 500 :initarg :height :accessor height)
   (%background :initform (#_new QColor 255 255 255) :initarg :background :accessor background)
   ;; Qt instances
   (%brush :initform (#_new QBrush) :accessor brush)
   (%pen :initform (#_new QPen) :accessor pen)
   (%pixmap :initform NIL :accessor pixmap)
   (%painter :initform NIL :accessor painter)
   ;;
   (%document :initarg :document :initform (error "Document required.") :accessor document)
   (%stroke :initform NIL :accessor stroke)))

(defmethod initialize-instance :after ((canvas canvas) &key)
  (#_setColor (brush canvas) (#_new QColor 0 0 0))
  (#_setColor (pen canvas) (#_new QColor 0 0 0))
  (#_setStyle (brush canvas) (#_Qt::SolidPattern))
  (#_setWidthF (pen canvas) 1)
  (#_setCapStyle (pen canvas) (#_Qt::RoundCap))
  (#_setJoinStyle (pen canvas) (#_Qt::RoundJoin))
  (resize-canvas canvas (width canvas) (height canvas)))

(defgeneric resize-canvas (canvas width height &optional x-offset y-offset)
  (:method ((canvas canvas) width height &optional (x-offset 0) (y-offset 0))
    (let ((pixmap (#_new QPixmap width height)))
      (#_fill pixmap (background canvas))
      (when (pixmap canvas)
        (#_end (painter canvas))
        (let ((painter (#_new QPainter pixmap)))
          (#_drawPixmap painter x-offset y-offset (pixmap canvas))
          (optimized-delete (pixmap canvas))
          (#_end painter)))
      (let ((painter (#_new QPainter pixmap)))
        (#_setRenderHint painter (#_QPainter::Antialiasing))
        (#_setRenderHint painter (#_QPainter::HighQualityAntialiasing))
        (#_setPen painter (pen canvas))
        (#_setBrush painter (brush canvas))
        
        (setf (pixmap canvas) pixmap
              (painter canvas) painter
              (width canvas) width
              (height canvas) height)))
    canvas))

(defgeneric scale-canvas (canvas width height)
  (:method ((canvas canvas) width height)
    ;; Dispose of old pixmap
    (let ((pixmap (#_scaled (pixmap canvas) width height (#_Qt::IgnoreAspectRatio) (#_Qt::SmoothTransformation))))
      (let ((painter (#_new QPainter pixmap)))
        (#_setRenderHint painter (#_QPainter::Antialiasing))
        (#_setRenderHint painter (#_QPainter::HighQualityAntialiasing))
        (#_setPen painter (pen canvas))
        (#_setBrush painter (brush canvas))
        
        (setf (pixmap canvas) pixmap
              (painter canvas) painter
              (width canvas) width
              (height canvas) height)))))

(defgeneric save-canvas (canvas path format)
  (:method ((canvas canvas) (path pathname) format)
    (#_save (pixmap canvas) (uiop:native-namestring path) format 100)
    path))

(defgeneric start-stroke (canvas x y x-tilt y-tilt pressure)
  (:method ((canvas canvas) x y x-tilt y-tilt pressure)
    (setf (stroke canvas) (make-instance 'stroke))
    (record-point canvas x y x-tilt y-tilt pressure)
    canvas))

(defmethod record-point ((canvas canvas) x y x-tilt y-tilt pressure)
  (record-point (stroke canvas) x y x-tilt y-tilt pressure)
  (draw-incremental (stroke canvas) (painter canvas))
  canvas)

(defmethod draw ((canvas canvas) painter)
  (#_drawPixmap painter 0 0 (pixmap canvas)))
