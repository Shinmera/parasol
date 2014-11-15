#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(named-readtables:in-readtable :qtools)

(define-tool (brush-tool "Brush" "Paint onto the canvas.") ()
  ((size :accessor size)
   (color :accessor color))
  (:options
    (size :type double-option :slot 'size :min 0.1 :max 100.0 :step 0.5)
    (color :type color-option :slot 'color :default (#_new QColor 0 0 0 255))))

