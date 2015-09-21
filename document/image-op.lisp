#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.document)
(named-readtables:in-readtable :qtools)

(defun make-painter (target)
  (let ((painter (painter target)))
    (setf (q+:render-hint painter) (q+:qpainter.antialiasing))
    (setf (q+:render-hint painter) (q+:qpainter.smooth-pixmap-transform))
    (setf (q+:render-hint painter) (q+:qpainter.high-quality-antialiasing))
    painter))

(defmacro with-painter ((painter target) &body body)
  `(with-finalizing ((,painter (make-painter ,target)))
     ,@body))
