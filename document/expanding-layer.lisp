#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defparameter *layer-block-size* 500)
(defparameter *layer-block-border* 200)

(defclass expanding-layer (raster-item)
  ())

(defmethod assure-suitable-size ((layer expanding-layer) x y)
  ;; First stroke. We can optimize by moving our canvas closer.
  (setf x (round x) y (round y))
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
