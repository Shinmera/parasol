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
   (%document :initform (error "Document required") :initarg :document :accessor document)))

(defmethod draw ((c cutoff) painter)
  (when (and (< 0 (width c))
             (< 0 (height c)))
    (let ((d (document c)))
      (#_fillRect painter
                  0
                  0
                  (+ (offset-x c) (offset-x d))
                  (#_height d)
                  (color c))
      (#_fillRect painter
                  (+ (offset-x c) (offset-x d) (width c))
                  0
                  (- (#_width d) (+ (offset-x c) (offset-x d) (width c)))
                  (#_height d)
                  (color c))
      (#_fillRect painter
                  (+ (offset-x c) (offset-x d))
                  0
                  (width c)
                  (+ (offset-y c) (offset-y d))
                  (color c))
      (#_fillRect painter
                  (+ (offset-x c) (offset-x d))
                  (+ (offset-y c) (offset-y d) (height c))
                  (width c)
                  (- (#_height d) (+ (offset-y c) (offset-y d) (height c)))
                  (color c)))))
