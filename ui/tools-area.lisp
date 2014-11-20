#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(with-widget-environment
  (define-widget tool-options-gizmo (QWidget)
    ())

  (define-subwidget label (#_new QLabel "Tool Options" widget))

  (define-subwidget layout (#_new QVBoxLayout widget)
    (#_setSizePolicy widget (#_QSizePolicy::Minimum) (#_QSizePolicy::Minimum))
    (#_addWidget layout label)
    (#_setLayout widget layout)
    (#_setMargin layout 0)
    (#_setSpacing layout 0)
    (#_setAlignment layout (#_Qt::AlignTop))))

(with-widget-environment
  (define-widget tools-area (QWidget)
    ())

  (define-subwidget gizmo (make-instance 'tool-options-gizmo)
    (add-widget gizmo (slot-value *window* 'gizmo-bar)))

  (define-subwidget layout (#_new QHBoxLayout widget)
    (#_setSizePolicy widget (#_QSizePolicy::Maximum) (#_QSizePolicy::Maximum))
    (#_setLayout widget layout)
    (#_setMargin layout 0)
    (#_setSpacing layout 0)
    (#_setAlignment layout (#_Qt::AlignLeft))
    (dolist (tool (find-tools))
      (#_addWidget layout (make-instance tool) 0)))

  (defmethod parasol-tools:select :after ((tool tool))
    (setf (tool *window*) tool)
    (let* ((tools-area (slot-value *window* 'tools-area))
           (gizmo (slot-value tools-area 'gizmo)))
      (with-slots-bound (gizmo tool-options-gizmo)
        (clear-layout layout)
        (#_addWidget layout label)
        (loop for (name . option) in (tool-options tool)
              do (#_addWidget layout option)))))

  (defmethod parasol-tools:deselect :after ((tool tool))
    (setf (tool *window*) NIL)
    (let* ((tools-area (slot-value *window* 'tools-area))
           (gizmo (slot-value tools-area 'gizmo)))
      (with-slots-bound (gizmo tool-options-gizmo)
        (clear-layout layout)
        (#_addWidget layout label)))))
