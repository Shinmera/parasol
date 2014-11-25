#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools.view)
(named-readtables:in-readtable :qtools)

(with-widget-environment
  (define-tool (zoom-tool "Zoom" "Zoom the canvas.") ()
    ((start-pos :initform NIL :accessor start-pos))
    (:icon :zoom ("mypaint-view-zoom"))))

(defmethod begin ((tool zoom-tool) pen document)
  (setf (start-pos tool) (- (- (/ (y-view pen) 1000)) (zoom (current-view)))))

(defmethod move ((tool zoom-tool) pen document)
  (setf (zoom (current-view)) (- (- (/ (y-view pen) 1000)) (start-pos tool))))

(defmethod end ((tool zoom-tool) pen document)
  (setf (start-pos tool) NIL))
