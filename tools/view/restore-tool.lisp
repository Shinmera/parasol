#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools.view)
(named-readtables:in-readtable :qtools)

(define-tool (restore-tool "Restore" "Restore all view options.") ()
  ())

(defmethod select ((tool restore-tool))
  (let ((view (current-view)))
    (setf (x view) 0
          (y view) 0
          (angle view) 0
          (zoom view) 1.0)
    (q+:repaint view)))
