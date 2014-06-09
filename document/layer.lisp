#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defparameter *layer-block-size* 500)
(defparameter *layer-block-border* 200)
;; Maybe switch to a different system
;; that uses a map of blocks instead of one
;; big one? But then how do I know where to draw...

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
      (format T "~a Fitting ~a,~a with new bounds ~a,~a ~a/~a~%" layer x y off-x off-y width height)
      (let* ((pixmap (#_new QImage width height (#_QImage::Format_ARGB32_Premultiplied)))
             (painter (#_new QPainter pixmap))
             (transparent (#_new QColor 0 0 255 10)))
        (#_fill pixmap transparent)
        (when (pixmap layer)
          (#_drawImage painter
                       (- (offset-x layer) off-x)
                       (- (offset-y layer) off-y)
                       (pixmap layer))
          (optimized-delete (pixmap layer)))
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
  (setf (stroke layer) (make-instance 'stroke))
  (record-point layer x y x-tilt y-tilt pressure)
  layer)

(defmethod record-point ((layer layer) x y x-tilt y-tilt pressure)
  (assure-suitable-size layer x y)
  (record-point (stroke layer) x y x-tilt y-tilt pressure)
  (draw-incremental (stroke layer) (painter layer))
  layer)

(defmethod draw ((layer layer) painter)
  (when (pixmap layer)
    (#_drawImage painter (offset-x layer) (offset-y layer) (pixmap layer))))

(defmethod finalize ((layer layer))
  (optimized-delete (painter layer))
  (optimized-delete (pixmap layer))
  (finalize (stroke layer)))
