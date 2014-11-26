#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(with-widget-environment
  (define-widget gizmo (QDockWidget)
    ())

  (define-initializer widget 100
    (#_setSizePolicy widget (#_QSizePolicy::Maximum) (#_QSizePolicy::Minimum))
    (#_setAllowedAreas widget (enum-or (#_Qt::LeftDockWidgetArea) (#_Qt::RightDockWidgetArea)))
    (#_addDockWidget *window* (#_Qt::RightDockWidgetArea) widget))

  (defgeneric refresh (gizmo)
    (:method ((gizmo gizmo)))))
