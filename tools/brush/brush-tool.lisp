#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools.brush)
(named-readtables:in-readtable :qtools)

(defclass stroke (drawable)
  ((points :initform (make-array 0 :element-type 'pen :adjustable T :fill-pointer 0) :accessor points)
   (brush :initform (error "BRUSH required.") :initarg :brush :accessor brush)))

(defmethod add-point ((pen pen) (stroke stroke))
  (vector-push-extend pen (points stroke))
  stroke)

(defmethod draw ((stroke stroke) target)
  (draw-stroke (brush stroke) stroke target))

(with-widget-environment
  (define-tool (brush-tool "Brush" "Paint onto the canvas.") ()
    ((current-brush :initform NIL :accessor current-brush))
    (:options
      (brush :type list-option :slot 'current-brush)
      (brush-options :type widget-option)))

  (defun %init-brush-tool-brushes (tool)
    (dolist (brush (or (find-brushes)
                       (warn "No brushes found.")))
      (add-item (string-downcase (class-name brush)) (tool-option 'brush tool))))

  (define-initializer tool 100
    (%init-brush-tool-brushes))

  ;; Hook into the definition of a new brush class.
  (defmethod initialize-instance :around ((brush brush) &key)
    (call-next-method)
    (%init-brush-tool-brushes)))

(defun translate-pen (pen)
  (let ((pen (copy pen)))
    (incf (x pen) (x (current-view)))
    (incf (y pen) (y (current-view)))
    pen))

(defmethod begin ((tool brush-tool) pen)
  (v:info :brush-tool "Beginning stroke at ~s" pen)
  (let ((stroke (make-instance 'stroke :brush (make-instance (or (find-symbol (string-upcase (current-brush tool)) "PARASOL")
                                                                 (error "Wtf. No brush like ~s found, but selected." (current-brush tool))))))
        (layer (current-layer (current-document)))
        (pen (translate-pen pen)))
    (ensure-fitting (x pen) (y pen) layer)
    (insert (add-point pen stroke) layer)
    (rebuffer layer)
    (#_repaint (current-view))))

(defmethod move ((tool brush-tool) pen)
  (let ((layer (current-layer (current-document)))
        (pen (translate-pen pen)))
    (add-point pen (current-drawable layer))
    (ensure-fitting (x pen) (y pen) layer)
    (rebuffer layer)
    (#_repaint (current-view))))

(defmethod end ((tool brush-tool) pen)
  )