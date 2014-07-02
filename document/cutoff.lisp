#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass cutoff ()
  ((%offset-x :initform 0 :accessor offset-x)
   (%offset-y :initform 0 :accessor offset-y)
   (%width :initform 0 :accessor width)
   (%height :initform 0 :accessor height)
   (%color :initform (#_new QColor 0 0 0 200) :accessor color)
   (%document :initform (error "Document required") :initarg :document :accessor document)
   (%user-defined :initform NIL :accessor user-defined)))

(defmethod draw ((c cutoff) painter)
  (when (and (< 0 (width c))
             (< 0 (height c))
             (user-defined c))
    (let ((d (document c)))
      (with-objects ((img (#_new QImage (#_width d) (#_height d) (#_QImage::Format_ARGB32_Premultiplied))))
        (#_fill img (color c))
        (with-painter (painter img)
          (#_setCompositionMode painter (#_QPainter::CompositionMode_Clear))
          (#_fillRect painter
                      (round (+ (offset-x d) (* (zoom d) (offset-x c))))
                      (round (+ (offset-y d) (* (zoom d) (offset-y c))))
                      (round (* (zoom d) (width c)))
                      (round (* (zoom d) (height c)))
                      (color c)))
        (#_drawImage painter 0 0 img)))))

(defmethod (setf user-defined) :after (new-value (cutoff cutoff))
  (#_update (document cutoff)))
