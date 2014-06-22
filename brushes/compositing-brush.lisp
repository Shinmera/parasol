#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass compositing-brush (abstract-brush)
  ((%name :initform "Compositing Brush" :accessor name)
   (%mode :initform 0 :initarg :mode :accessor mode))
  (:metaclass brush-class)
  (:fields (mode :type :choice :choices ("Normal" "Erase" "Lock Alpha" "Multiply" "Screen" "Overlay" "Darken" "Lighten" "Dodge" "Burn" "Hard Light" "Soft Light" "Difference" "Exclusion") :slot %mode
                 :setter #'(lambda (slot value)
                             (set-brush-slot slot (gethash value *compositing-mode-map*))))))

(defmethod draw-whole :around ((brush compositing-brush) painter pixmap)
  (let ((old-mode (#_compositionMode painter)))
    (#_setCompositionMode painter (mode brush))
    (call-next-method)
    (#_setCompositionMode painter old-mode)))

(defclass composited-brush (compositing-brush brush)
  ((%name :initform "Compositing Brush" :accessor name))
  (:metaclass brush-class))
