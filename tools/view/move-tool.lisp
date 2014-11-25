#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools.view)
(named-readtables:in-readtable :qtools)

(with-widget-environment
  (define-tool (move-tool "Move" "Move the canvas.") ()
    ((start-pos :initform NIL :accessor start-pos))))

(defmethod begin ((tool move-tool) pen document)
  (setf (start-pos tool) (cons (- (x-view pen) (x (current-view)))
                               (- (y-view pen) (y (current-view))))))

(defmethod move ((tool move-tool) pen document)
  (setf (x (current-view)) (- (x-view pen) (car (start-pos tool))))
  (setf (y (current-view)) (- (y-view pen) (cdr (start-pos tool)))))

(defmethod end ((tool move-tool) pen document)
  (setf (start-pos tool) NIL))
