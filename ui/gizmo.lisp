#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(define-widget gizmo (QDockWidget)
  ())

(define-initializer (gizmo setup)
  (setf (q+:size-policy gizmo) (q+:qsizepolicy.maximum) (q+:qsizepolicy.minimum))
  (setf (q+:allowed-areas gizmo) (enum-or (q+:qt.left-dock-widget-area) (q+:qt.right-dock-widget-area)))
  (q+:add-dock-widget *window* (q+:qt.right-dock-widget-area) gizmo))

(defgeneric refresh (gizmo)
  (:method ((gizmo gizmo))))
