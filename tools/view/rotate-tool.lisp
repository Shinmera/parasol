#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools.view)
(named-readtables:in-readtable :qtools)

(define-tool (rotate-tool "Rotate" "Rotate the canvas.") ()
  ((start-pos :initform NIL :accessor start-pos))
  (:icon :rotate ("mypaint-view-rotate")))

(defmethod begin ((tool rotate-tool) pen document)
  (setf (start-pos tool) (- (y-view pen) (angle (current-view)))))

(defmethod move ((tool rotate-tool) pen document)
  (setf (angle (current-view)) (- (y-view pen) (start-pos tool))))

(defmethod end ((tool rotate-tool) pen document)
  (setf (start-pos tool) NIL))
