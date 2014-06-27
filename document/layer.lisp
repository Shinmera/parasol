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
   (%current-stroke :initform NIL :accessor current-stroke)
   (%history-size :initform 0 :accessor history-size)
   (%mode :initarg :mode :initform 0 :accessor mode)
   (%opacity :initarg :opacity :initform 1.0 :accessor opacity)
   (%name :initarg :name :accessor name)))

(defmethod print-object ((layer layer) stream)
  (print-unreadable-object (layer stream :type T :identity T)
    (format stream "~a" (name layer)))
  layer)

(defmethod start-stroke ((layer layer) type x y x-tilt y-tilt pressure)
  (v:debug :layer "~a Starting stroke" layer)
  (truncate-history layer)
  (setf (current-stroke layer) (make-instance 'stroke))
  (record-point layer x y x-tilt y-tilt pressure)
  layer)

(defmethod record-point ((layer layer) x y x-tilt y-tilt pressure)
  (assure-suitable-size layer x y)
  (record-point (current-stroke layer) x y x-tilt y-tilt pressure)
  layer)

(defmethod end-stroke ((layer layer))
  (v:debug :layer "~a Ending stroke" layer)
  (push-history-item layer (current-stroke layer))
  (setf (current-stroke layer) NIL))

(defmethod push-history-item ((layer layer) (item raster-item))
  (assure-suitable-size layer (offset-x item) (offset-y item))
  (assure-suitable-size layer
                        (+ (offset-x item) (width item))
                        (+ (offset-y item) (height item)))
  (vector-push-extend item (strokes layer))
  (with-transform ((painter layer))
    (draw item (painter layer))))

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
    (v:debug :layer "~a History now at ~a/~a"
             layer (fill-pointer (strokes layer)) (1+ (history-size layer)))
    (recache layer)))

(defmethod redo ((layer layer))
  (when (<= (fill-pointer (strokes layer)) (history-size layer))
    (incf (fill-pointer (strokes layer)))
    (v:debug :layer "~a History now at ~a/~a"
             layer (fill-pointer (strokes layer)) (1+ (history-size layer)))
    (recache layer)))

(defmethod recache ((layer layer))
  (with-objects ((transparent (#_new QColor 0 0 0 0)))
    (#_fill (pixmap layer) transparent)
    (loop for stroke across (strokes layer)
          do (with-transform ((painter layer))
               (draw stroke (painter layer))))))

(defmethod draw ((layer layer) painter)
  (when (pixmap layer)
    (#_setOpacity painter (opacity layer))
    (#_setCompositionMode painter (mode layer))
    (if (current-stroke layer)
        (progn
          (with-objects ((tempimg (#_new QImage (pixmap layer)))
                         (temppaint (#_new QPainter tempimg)))
            (#_translate temppaint (- (offset-x layer)) (- (offset-y layer)))
            (draw (current-stroke layer) temppaint)
            (#_drawImage painter (offset-x layer) (offset-y layer) tempimg)))
        (#_drawImage painter (offset-x layer) (offset-y layer) (pixmap layer)))))

(defmethod find-real-size ((layer layer))
  ;; THIS IS INEXCUSABLY INEFFICIENT
  (loop with (fx fy lx ly) = `(,most-positive-fixnum ,most-positive-fixnum
                               ,most-negative-fixnum ,most-negative-fixnum)
        for x from 0 below (width layer)
        do (loop for y from 0 below (height layer)
                 for rgb of-type (unsigned-byte 32) = (#_pixel (pixmap layer) x y)
                 do (when (/= (ldb (byte 8 24) rgb) 0)
                      ;; I would've liked to use QGlobalSpace::qAlpha above, but apparently
                      ;; that isn't possible in CommonQt for unknown reasons, so we hard-code it.
                      (When (< x fx) (setf fx x))
                      (when (< y fy) (setf fy y))
                      (when (> x lx) (setf lx x))
                      (when (> y ly) (setf ly y))))
        finally (return (list (+ fx (offset-x layer))
                              (+ fy (offset-y layer))
                              (+ lx (offset-x layer))
                              (+ ly (offset-y layer))))))

(defmethod finalize ((layer layer))
  (cleanup (layer) current-stroke)
  (loop for stroke across (strokes layer)
        do (finalize stroke))
  (call-next-method))
