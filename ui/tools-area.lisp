#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(define-widget tool-options-gizmo (QDockWidget)
  ())

(define-subwidget (tool-options-gizmo central) (#_new QWidget tool-options-gizmo)
  (#_setWindowTitle tool-options-gizmo "Tool Options")
  (#_setWidget tool-options-gizmo central))

(define-subwidget (tool-options-gizmo layout) (#_new QVBoxLayout tool-options-gizmo)
  (#_setSizePolicy tool-options-gizmo (#_QSizePolicy::Maximum) (#_QSizePolicy::Minimum))
  (#_setMinimumWidth tool-options-gizmo 150)
  (#_setLayout central layout)
  (#_setMargin layout 0)
  (#_setSpacing layout 0)
  (#_setAlignment layout (#_Qt::AlignTop)))

(define-widget tools-area (QToolBar)
  ())

(define-subwidget (tools-area group) (#_new QButtonGroup tools-area))

(define-subwidget (tools-area gizmo) (make-instance 'tool-options-gizmo)
  ;; (add-widget gizmo (slot-value *window* 'gizmo-bar))
  (#_addDockWidget *window* (#_Qt::RightDockWidgetArea) gizmo))

(define-initializer (tools-area setup)
  (dolist (tool (find-tools))
    (let ((tool (make-instance tool)))
      (#_addButton group tool)
      (#_addWidget tools-area tool)))
  (#_addToolBar *window* tools-area))

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
      (clear-layout layout))))
