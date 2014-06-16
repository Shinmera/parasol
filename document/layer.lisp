#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defparameter *layer-block-size* 500)
(defparameter *layer-block-border* 200)
(defparameter *presample-history* 20)

(defclass layer ()
  ((%width :initform 0 :accessor width)
   (%height :initform 0 :accessor height)
   (%offset-x :initform 0 :accessor offset-x)
   (%offset-y :initform 0 :accessor offset-y)

   (%pixmap :initform NIL :accessor pixmap)
   (%painter :initform NIL :accessor painter)
   (%strokes :initform (make-array *presample-history* :adjustable T :fill-pointer 0) :accessor strokes)
   (%history-size :initform 0 :accessor history-size)
   (%mode :initarg :mode :accessor mode)
   (%name :initarg :name :accessor name)))

(defmethod print-object ((layer layer) stream)
  (print-unreadable-object (layer stream :type T :identity T)
    (format stream "~a" (name layer)))
  layer)

(defmethod initialize-instance :after ((layer layer) &key))

(defmethod assure-suitable-size ((layer layer) x y)
  ;; First stroke. We can optimize by moving our canvas closer.
  (when (= (width layer) 0)
    (setf (offset-x layer) (+ x (/ *layer-block-size* 2))
          (offset-y layer) (+ y (/ *layer-block-size* 2))))
  (unless (and (< (+ (offset-x layer)
                     *layer-block-border*)
                  x
                  (- (+ (offset-x layer) (width layer))
                     *layer-block-border*))
               (< (+ (offset-y layer)
                     *layer-block-border*)
                  y
                  (- (+ (offset-y layer) (height layer))
                     *layer-block-border*)))
    (let* ((off-x (offset-x layer))
           (off-y (offset-y layer))
           (required-width  (* *layer-block-size* (ceiling (/ (+ (abs (- x off-x)) *layer-block-border*) *layer-block-size*))))
           (required-height (* *layer-block-size* (ceiling (/ (+ (abs (- y off-y)) *layer-block-border*) *layer-block-size*))))
           (width (max (width layer)
                       (if (<= (+ off-x *layer-block-size*) x)
                           required-width
                           (progn (decf off-x required-width)
                                  (+ (width layer) required-width)))))
           (height (max (height layer)
                        (if (<= (+ off-y *layer-block-size*) y)
                            required-height
                            (progn (decf off-y required-height)
                                   (+ (height layer) required-height))))))
      (v:debug :layer "~a Fitting ~a,~a with new bounds ~a,~a ~a/~a~%" layer x y off-x off-y width height)
      (let* ((pixmap (#_new QImage width height (#_QImage::Format_ARGB32_Premultiplied)))
             (painter (#_new QPainter pixmap))
             (transparent (#_new QColor 0 0 0 0)))
        (#_fill pixmap transparent)
        (when (pixmap layer)
          (#_drawImage painter
                       (- (offset-x layer) off-x)
                       (- (offset-y layer) off-y)
                       (pixmap layer))
          (maybe-delete-qobject (pixmap layer)))
        (#_setRenderHint painter (#_QPainter::Antialiasing))
        (#_setRenderHint painter (#_QPainter::HighQualityAntialiasing))
        (#_translate painter (- off-x) (- off-y))
        (#_setStyle (#_brush painter) (#_Qt::SolidPattern))
        (setf (pixmap layer) pixmap
              (painter layer) painter
              (offset-x layer) off-x
              (offset-y layer) off-y
              (width layer) width
              (height layer) height)))))

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

(defmethod recache ((layer layer))
  (with-objects ((transparent (#_new QColor 0 0 0 0)))
    (#_fill (pixmap layer) transparent)
    (loop for stroke across (strokes layer)
          do (draw stroke (painter layer)))))

(defmethod undo ((layer layer))
  (when (< 0 (fill-pointer (strokes layer)))
    (decf (fill-pointer (strokes layer)))
    (recache layer)))

(defmethod redo ((layer layer))
  (when (< (fill-pointer (strokes layer)) (history-size layer))
    (incf (fill-pointer (strokes layer)))
    (recache layer)))

(defmethod draw ((layer layer) painter)
  (when (pixmap layer)
    (#_drawImage painter (offset-x layer) (offset-y layer) (pixmap layer))))

(defmethod finalize ((layer layer))
  (maybe-delete-qobject (painter layer))
  (maybe-delete-qobject (pixmap layer))
  (loop for stroke across (strokes layer)
        do (finalize stroke))
  (setf (painter layer) NIL
        (pixmap layer) NIL
        (strokes layer) NIL))
