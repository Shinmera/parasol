#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(with-widget-environment
  (define-widget gizmo-bar (QWidget)
    ())

  (define-subwidget layout (#_new QVBoxLayout widget)
    (#_setLayout widget layout)
    (#_setAlignment layout (#_Qt::AlignTop))
    (#_setMinimumSize widget 200 0))

  (defun add-widget (widget gizmo-bar)
    (#_addWidget (slot-value gizmo-bar 'layout) widget)))
