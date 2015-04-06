#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools.brush)
(named-readtables:in-readtable :qtools)

(define-finalizable stroke (adaptive-buffered)
  ((points :initform (make-array 0 :element-type 'pen :adjustable T :fill-pointer 0) :accessor points)
   (brush :initform (error "BRUSH required.") :initarg :brush :accessor brush)))

(defgeneric add-point (pen stroke)
  (:method ((pen pen) (stroke stroke))
    (vector-push-extend pen (points stroke))
    (ensure-fitting (x pen) (y pen) stroke)
    (when (< 1 (length (points stroke)))
      (rebuffer-incremental stroke (- (length (points stroke)) 2)))
    stroke))

(defgeneric rebuffer-incremental (stroke from)
  (:method ((stroke stroke) from)
    (let ((painter (painter stroke)))
      (with-translation-away (stroke painter)
        (draw-stroke (brush stroke) stroke painter from)))))

(defmethod draw-buffer ((stroke stroke) target)
  (draw-stroke (brush stroke) stroke target 0))

(defun switch-brush (tool option brush-name)
  (declare (ignore option))
  (when (and brush-name (string/= brush-name ""))
    (let ((previous (current-brush tool)))
      (when previous
        (finalize previous)))
    (let ((brush (make-instance (or (find-symbol (string-upcase brush-name) #.*package*)
                                    (error "Wtf. No brush like ~s found, but selected." brush-name))))
          (layout (slot-value (tool-option 'brush-options tool) 'parasol-tools::layout)))
      (v:info :brush-tool "Selecting brush ~s" brush)
      (clear-layout layout)
      (loop for (name . option) in (brush-options brush)
            do (#_addWidget layout option))
      brush)))

(define-tool (brush-tool "Brush" "Paint onto the canvas.") ()
  ((current-brush :initform NIL :accessor current-brush :finalized T))
  (:display brush brush-options))

(define-initializer (brush-tool setup)
  (dolist (brush (or (find-brushes)
                     (warn "No brushes found.")))
    (add-item (string-downcase (class-name brush))
              (tool-option 'brush brush-tool))))

(defmethod begin ((tool brush-tool) pen document)
  (v:info :brush-tool "Beginning stroke at ~s" pen)
  (let ((stroke (make-instance 'stroke :brush (copy (current-brush tool))
                                       :x (x pen) :y (y pen)))
        (layer (current-layer document)))
    (insert (add-point pen stroke) layer)
    ;; FIXME! need to be turned into a call to the render loop once we have that.
    ;; FIXME²! We also need to optimise this to not rebuffer completely because that's insane.
    ;; Instead we need to add buffer drawing modes and then only do incremental painting.
    ;; FIXME³! Also, we need some kind of way to notify higher-level caches of the changes.
    ))

(defmethod move ((tool brush-tool) pen document)
  (let ((layer (current-layer document)))
    (add-point pen (current-drawable layer))))

(defmethod end ((tool brush-tool) pen document)
  ;; History hook here some day.
  )
