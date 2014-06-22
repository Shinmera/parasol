#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defparameter *presample-history* 20)

(defclass layer (expanding-layer)
  ((%strokes :initform (make-array *presample-history* :adjustable T :fill-pointer 0) :accessor strokes)
   (%history-size :initform 0 :accessor history-size)
   (%mode :initarg :mode :initform 0 :accessor mode)
   (%opacity :initarg :opacity :initform 1.0 :accessor opacity)
   (%name :initarg :name :accessor name)))

(defmethod print-object ((layer layer) stream)
  (print-unreadable-object (layer stream :type T :identity T)
    (format stream "~a" (name layer)))
  layer)

(defmethod start-stroke ((layer layer) type x y x-tilt y-tilt pressure)
  (truncate-history layer)
  (vector-push-extend (make-instance 'stroke) (strokes layer))
  (record-point layer x y x-tilt y-tilt pressure)
  layer)

(defmethod record-point ((layer layer) x y x-tilt y-tilt pressure)
  (assure-suitable-size layer x y)
  (let ((stroke (elt (strokes layer) (1- (fill-pointer (strokes layer))))))
    (record-point stroke x y x-tilt y-tilt pressure)
    (draw-incremental stroke (painter layer)))
  layer)

(defmethod end-stroke ((layer layer))
  )

(defmethod truncate-history ((layer layer))
  (loop for i from (fill-pointer (strokes layer))
          to (history-size layer)
        for stroke = (aref (strokes layer) i)
        when stroke
          do (finalize stroke)
             (setf (aref (strokes layer) i) NIL))
  (setf (history-size layer) (fill-pointer (strokes layer))))

;; These need to become much much snappier.
;; Suggested strategy: Cache chunks of history items and break up when needed.
;; A library that offers this would be a good idea.
(defmethod undo ((layer layer))
  (when (< 0 (fill-pointer (strokes layer)))
    (decf (fill-pointer (strokes layer)))
    (recache layer)))

(defmethod redo ((layer layer))
  (when (< (fill-pointer (strokes layer)) (history-size layer))
    (incf (fill-pointer (strokes layer)))
    (recache layer)))

(defmethod recache ((layer layer))
  (with-objects ((transparent (#_new QColor 0 0 0 0)))
    (#_fill (pixmap layer) transparent)
    (loop for stroke across (strokes layer)
          do (draw stroke (painter layer)))))

(defmethod draw ((layer layer) painter)
  (when (pixmap layer)
    (#_setOpacity painter (opacity layer))
    (#_setCompositionMode painter (mode layer))
    (#_drawImage painter (offset-x layer) (offset-y layer) (pixmap layer))))

(defmethod finalize ((layer layer))
  (maybe-delete-qobject (painter layer))
  (maybe-delete-qobject (pixmap layer))
  (loop for stroke across (strokes layer)
        do (finalize stroke))
  (setf (painter layer) NIL
        (pixmap layer) NIL
        (strokes layer) NIL))
