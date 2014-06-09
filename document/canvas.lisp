#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass canvas ()
  ((%bg-brush :initform NIL :initarg :bg-brush :accessor bg-brush)
   (%offset-x :initform 0 :accessor offset-x)
   (%offset-y :initform 0 :accessor offset-y)
   (%document :initarg :document :initform (error "Document required.") :accessor document)
   (%active-layer :initform 0 :accessor active-layer)
   (%layers :initform (make-array 0 :adjustable T :fill-pointer 0) :accessor layers)
   (%stroke :initform NIL :accessor stroke)))

(defmethod initialize-instance :after ((canvas canvas) &key)
  (setf (background canvas) (merge-pathnames "background.png" (asdf:system-source-directory :parasol)))
  (add-layer canvas))

(defgeneric resize-canvas (canvas width height &optional x-offset y-offset)
  (:method ((canvas canvas) width height &optional (x-offset 0) (y-offset 0))
    ))

(defgeneric scale-canvas (canvas width height)
  (:method ((canvas canvas) width height)
    ))

(defgeneric save-canvas (canvas path format)
  (:method ((canvas canvas) (path pathname) format)
    ))

(defmethod start-stroke ((canvas canvas) type x y x-tilt y-tilt pressure)
  (start-stroke (aref (layers canvas) (active-layer canvas))
                type
                (- x (offset-x canvas))
                (- y (offset-y canvas))
                x-tilt y-tilt pressure)
  canvas)

(defmethod record-point ((canvas canvas) x y x-tilt y-tilt pressure)
  (record-point (aref (layers canvas) (active-layer canvas))
                (- x (offset-x canvas))
                (- y (offset-y canvas))
                x-tilt y-tilt pressure)
  canvas)

(defmethod draw ((canvas canvas) painter)
  (#_fillRect painter (#_rect (document canvas)) (bg-brush canvas))
  (#_translate painter (offset-x canvas) (offset-y canvas))
  (loop for layer across (layers canvas)
        do (draw layer painter)))

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
    layer)
  (#_update (document canvas)))

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
         (index (min index (1- (length layers))))
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
          (active-layer canvas) index))
  (#_update (document canvas)))

(defmethod move ((canvas canvas) x y)
  (incf (offset-x canvas) x)
  (incf (offset-y canvas) y))

(defmethod finalize ((canvas canvas))
  (remove-canvas-background canvas)
  (optimized-delete (brush canvas))
  (optimized-delete (pen canvas))
  (optimized-delete (painter canvas))
  (optimized-delete (pixmap canvas))
  (loop for layer across (layers canvas)
        do (finalize layer)))
