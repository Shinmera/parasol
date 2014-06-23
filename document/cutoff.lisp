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
   (%canvas :initform (error "Canvas required") :initarg :canvas :accessor canvas)))

(defmethod draw ((c cutoff) painter)
  (when (and (< 0 (width c))
             (< 0 (height c)))
    (let ((canvas (canvas c)))
      (#_fillRect painter
                  0
                  0
                  (+ (offset-x c) (offset-x canvas))
                  (#_height (document canvas))
                  (color c))
      (#_fillRect painter
                  (+ (offset-x c) (offset-x canvas) (width c))
                  0
                  (- (#_width (document canvas)) (+ (offset-x c) (offset-x canvas) (width c)))
                  (#_height (document canvas))
                  (color c))
      (#_fillRect painter
                  (+ (offset-x c) (offset-x canvas))
                  0
                  (width c)
                  (+ (offset-y c) (offset-y canvas))
                  (color c))
      (#_fillRect painter
                  (+ (offset-x c) (offset-x canvas))
                  (+ (offset-y c) (offset-y canvas) (height c))
                  (width c)
                  (- (#_height (document canvas)) (+ (offset-y c) (offset-y canvas) (height c)))
                  (color c))))))
