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
   (%active-layer-index :initform 0 :accessor active-layer-index)
   (%layers :initform (make-array 0 :adjustable T :fill-pointer 0) :accessor layers)))

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
  (push-color *window*)
  (start-stroke (active-layer canvas)
                type
                (- x (offset-x canvas))
                (- y (offset-y canvas))
                x-tilt y-tilt pressure)
  canvas)

(defmethod record-point ((canvas canvas) x y x-tilt y-tilt pressure)
  (record-point (active-layer canvas)
                (- x (offset-x canvas))
                (- y (offset-y canvas))
                x-tilt y-tilt pressure)
  canvas)

(defmethod end-stroke ((canvas canvas))
  (end-stroke (active-layer canvas)))

(defmethod draw ((canvas canvas) painter)
  (#_fillRect painter (#_rect (document canvas)) (bg-brush canvas))
  (#_translate painter (offset-x canvas) (offset-y canvas))
  (loop for layer across (layers canvas)
        do (draw layer painter)))

(defun remove-canvas-background (canvas)
  (let ((bg-brush (bg-brush canvas)))
    (when bg-brush
      (maybe-delete-qobject (#_textureImage bg-brush))
      ;;(maybe-delete-qobject (#_color bg-brush))
      (maybe-delete-qobject bg-brush))))

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

(defmethod active-layer ((canvas canvas))
  (aref (layers canvas) (active-layer-index canvas)))

(defmethod (setf active-layer) (index (canvas canvas))
  (setf (active-layer-index canvas) index))

(defmethod move-layer ((canvas canvas) index)
  (let* ((layers (layers canvas))
         (index (min index (1- (length layers))))
         (layer (active-layer canvas)))
    ;; Pop out
    (loop for i from (active-layer-index canvas) below (1- (length layers))
          do (setf (aref layers i)
                   (aref layers (1+ i))))
    ;; Shift
    (loop for i downfrom (length layers) above index
          do (setf (aref layers i)
                   (aref layers (1- i))))
    ;; Push in
    (setf (aref layers index) layer
          (active-layer-index canvas) index))
  (#_update (document canvas)))

(defmethod move ((canvas canvas) x y)
  (incf (offset-x canvas) x)
  (incf (offset-y canvas) y))

(defmethod undo ((canvas canvas))
  (undo (active-layer canvas))
  (#_update (document canvas)))

(defmethod redo ((canvas canvas))
  (redo (active-layer canvas))
  (#_update (document canvas)))

(defmethod finalize ((canvas canvas))
  (remove-canvas-background canvas)
  (loop for layer across (layers canvas)
        do (finalize layer))
  (setf (bg-brush canvas) NIL
        (layers canvas) NIL
        (document canvas) NIL))
