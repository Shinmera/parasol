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
   (%bg-brush :initform NIL :initarg :bg-brush :accessor bg-brush)
   ;; Qt instances
   (%brush :initform (#_new QBrush) :accessor brush)
   (%pen :initform (#_new QPen) :accessor pen)
   (%pixmap :initform NIL :accessor pixmap)
   (%painter :initform NIL :accessor painter)
   ;;
   (%document :initarg :document :initform (error "Document required.") :accessor document)
   (%active-layer :initform 0 :accessor active-layer)
   (%layers :initform (make-array 0 :adjustable T :fill-pointer 0) :accessor layers)
   (%stroke :initform NIL :accessor stroke)))

(defmethod initialize-instance :after ((canvas canvas) &key)
  (#_setColor (brush canvas) (#_new QColor 0 0 0))
  (#_setColor (pen canvas) (#_new QColor 0 0 0))
  (#_setStyle (brush canvas) (#_Qt::SolidPattern))
  (#_setWidthF (pen canvas) 1)
  (#_setCapStyle (pen canvas) (#_Qt::RoundCap))
  (#_setJoinStyle (pen canvas) (#_Qt::RoundJoin))
  (setf (background canvas) (merge-pathnames "background.png" (asdf:system-source-directory :parasol)))
  (add-layer canvas)
  (resize-canvas canvas (width canvas) (height canvas)))

(defgeneric resize-canvas (canvas width height &optional x-offset y-offset)
  (:method ((canvas canvas) width height &optional (x-offset 0) (y-offset 0))
    (let ((pixmap (#_new QPixmap width height)))
      (#_fill pixmap (#_new QColor 0 0 0 0))
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

(defgeneric start-stroke (canvas type x y x-tilt y-tilt pressure)
  (:method ((canvas canvas) type x y x-tilt y-tilt pressure)
    (setf (stroke canvas) (make-instance 'stroke))
    (record-point canvas x y x-tilt y-tilt pressure)
    canvas))

(defmethod record-point ((canvas canvas) x y x-tilt y-tilt pressure)
  (record-point (stroke canvas) x y x-tilt y-tilt pressure)
  (draw-incremental (stroke canvas) (painter canvas))
  canvas)

(defmethod draw ((canvas canvas) painter)
  (#_fillRect painter (#_rect (document canvas)) (bg-brush canvas))
  (#_drawPixmap painter 0 0 (pixmap canvas)))

(defun remove-canvas-background (canvas)
  (let ((bg-brush (bg-brush canvas)))
    (when bg-brush
      (unless (qt:null-qobject-p (#_textureImage bg-brush))
        (optimized-delete (#_textureImage bg-brush)))
      (unless (qt:null-qobject-p (#_color bg-brush))
        (optimized-delete (#_color bg-brush)))
      (optimized-delete bg-brush))))

(defmethod (setf background) ((file pathname) (canvas canvas))
  (remove-canvas-background canvas)
  (setf (bg-brush canvas)
        (#_new QBrush (#_new QImage (uiop:native-namestring file)))))

(defmethod (setf background) ((rgb list) (canvas canvas))
  (remove-canvas-background canvas)
  (setf (br-brush canvas)
        (#_new QBrush (#_new QColor (first rgb) (second rgb) (third rgb)))))

(defmethod (setf background) (brush-parameter (canvas canvas))
  (remove-canvas-background canvas)
  (setf (bg-brush canvas)
        (#_new QBrush brush-parameter)))

(defmethod add-layer ((canvas canvas) &key name mode)
  (let ((layer (make-instance 'layer :name (or name (format NIL "Layer ~d" (length (layers canvas)))) :mode mode)))
    (vector-push-extend layer (layers canvas))
    layer))

(defmethod remove-layer ((canvas canvas) &optional index)
  (when index
    (destroy (elt (layers canvas) index))
    (loop for i from index below (1- (length (layers canvas)))
          do (setf (aref (layers canvas) i)
                   (aref (layers canvas) (1+ i))))
    (vector-pop (layers canvas))
    (when (= (length (layers canvas)) 0)
      (add-layer canvas))
    (#_update (document canvas))))

(defmethod activate-layer ((canvas canvas) index)
  (setf (active-layer canvas) index))

(defmethod move-layer ((canvas canvas) index)
  (let* ((layers (layers canvas))
         (layer (aref layers (active-layer canvas))))
    ;; Pop out
    (loop for i from (active-layer canvas) below (1- (length layers))
          do (setf (aref layers i)
                   (aref layers (1+ i))))
    ;; Shift
    (loop for i downfrom (length layers) above index
          do (setf (aref layers i)
                   (aref layers (1- i))))
    ;; Push in
    (setf (aref layers index) layer
          (active-layer canvas) index)))

(defmethod finalize ((canvas canvas))
  (remove-canvas-background canvas)
  (optimized-delete (brush canvas))
  (optimized-delete (pen canvas))
  (optimized-delete (painter canvas))
  (optimized-delete (pixmap canvas))
  (loop for layer across (layers canvas)
        do (finalize layer)))
