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

(defun populate-tool-options (gizmo tool)
  (let ((layout (slot-value gizmo 'layout)))
    (clear-layout layout)
    (q+:add-widget layout (make-input-for-configurable tool))))

(define-widget tool-button (QPushButton)
  ((tool :initarg :tool :accessor tool))
  (:default-initargs
    :tool (error "TOOL required.")))

(define-initializer (tool-button setup)
  (setf (q+:checkable tool-button) T)
  (setf (q+:text tool-button) (tool-title tool)))

(define-slot (tool-button changed) ((checked bool))
  (declare (connected tool-button (toggled bool)))
  (let ((tool (tool tool-button)))
    (cond (checked
           (setf (tool *window*) tool))
          (T
           (deselect tool)))))

(define-widget tools-area (QToolBar)
  ((button-map :initform (make-hash-table :test 'eql) :accessor button-map)))

(define-subwidget (tools-area group) (#_new QButtonGroup tools-area)
  (setf (q+:exclusive group) T))

(define-subwidget (tools-area gizmo) (make-instance 'tool-options-gizmo)
  (#_addDockWidget *window* (#_Qt::RightDockWidgetArea) gizmo))

(define-initializer (tools-area setup)
  (dolist (tool (find-tools))
    (let* ((tool (make-instance tool))
           (button (make-instance 'tool-button :tool tool)))
      (q+:add-button group button)
      (q+:add-widget tools-area button)
      (setf (gethash tool button-map) button)))
  (setf (q+:window-title tools-area) "Tools")
  (q+:add-tool-bar *window* tools-area))

(defmethod (setf tool) :around (tool (window main-window))
  (unless (eql tool (tool window))
    (call-next-method)
    (let ((gizmo (slot-value (slot-value window 'tools-area) 'gizmo)))
      (populate-tool-options gizmo tool))
    (select tool)
    (let ((button (gethash tool (button-map (slot-value window 'tools-area)))))
      (if button
          (q+:click button)
          (v:warn :tools-area "Setting tool ~s which has no corresponding button!" tool)))))

(defmethod configurable-slot-changed :after ((tool tool) slot)
  (when (and (eql (c2mop:slot-definition-type slot)
                  'configurable)
             (slot-boundp *window* 'tools-area))
    (let ((gizmo (slot-value (slot-value *window* 'tools-area) 'gizmo)))
      ;; (populate-tool-options gizmo tool)
      )))
