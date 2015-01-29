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
  (#_setSizePolicy gizmo (#_QSizePolicy::Maximum) (#_QSizePolicy::Minimum))
  (#_setAllowedAreas gizmo (enum-or (#_Qt::LeftDockWidgetArea) (#_Qt::RightDockWidgetArea)))
  (#_addDockWidget *window* (#_Qt::RightDockWidgetArea) gizmo))

(defgeneric refresh (gizmo)
  (:method ((gizmo gizmo))))
