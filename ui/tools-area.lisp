#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(with-widget-environment
  (define-widget tool-options-gizmo (QDockWidget)
    ())

  (define-subwidget central (#_new QWidget widget)
    (#_setWindowTitle widget "Tool Options")
    (#_setWidget widget central))

  (define-subwidget layout (#_new QVBoxLayout widget)
    (#_setSizePolicy widget (#_QSizePolicy::Maximum) (#_QSizePolicy::Minimum))
    (#_setMinimumWidth widget 150)
    (#_setLayout central layout)
    (#_setMargin layout 0)
    (#_setSpacing layout 0)
    (#_setAlignment layout (#_Qt::AlignTop))))

(with-widget-environment
  (define-widget tools-area (QToolBar)
    ())

  (define-subwidget group (#_new QButtonGroup widget))

  (define-subwidget gizmo (make-instance 'tool-options-gizmo)
    ;; (add-widget gizmo (slot-value *window* 'gizmo-bar))
    (#_addDockWidget *window* (#_Qt::RightDockWidgetArea) gizmo))

  (define-initializer widget 100
    (dolist (tool (find-tools))
      (let ((tool (make-instance tool)))
        (#_addButton group tool)
        (#_addWidget widget tool)))
    (#_addToolBar *window* widget))

  (defmethod parasol-tools:select :after ((tool tool))
    (setf (tool *window*) tool)
    (let* ((tools-area (slot-value *window* 'tools-area))
           (gizmo (slot-value tools-area 'gizmo)))
      (with-slots-bound (gizmo tool-options-gizmo)
        (clear-layout layout)
        (loop for (name . option) in (tool-options tool)
              do (#_addWidget layout option)))))

  (defmethod parasol-tools:deselect :after ((tool tool))
    (setf (tool *window*) NIL)
    (let* ((tools-area (slot-value *window* 'tools-area))
           (gizmo (slot-value tools-area 'gizmo)))
      (with-slots-bound (gizmo tool-options-gizmo)
        (clear-layout layout)))))
