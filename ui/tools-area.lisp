#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(define-widget tool-options-gizmo (QDockWidget)
  ())

(define-subwidget (tool-options-gizmo central) (q+:make-qwidget tool-options-gizmo)
  (setf (q+:window-title tool-options-gizmo) "Tool Options")
  (setf (q+:widget tool-options-gizmo) central))

(define-subwidget (tool-options-gizmo layout) (q+:make-qvboxlayout tool-options-gizmo)
  (setf (q+:size-policy tool-options-gizmo) (values (q+:qsizepolicy.maximum) (q+:qsizepolicy.minimum)))
  (setf (q+:minimum-width tool-options-gizmo) 150)
  (setf (q+:layout central) layout)
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 0)
  (setf (q+:alignment layout) (q+:qt.align-top)))

(defvar *populating* NIL)

(defun populate-tool-options (gizmo tool)
  (unless *populating*
    (let ((layout (slot-value gizmo 'layout))
          (*populating* T))
      (clear-layout layout)
      (q+:add-widget layout (make-input-for-configurable tool)))))

(define-widget tool-button (QPushButton)
  ((tool :initarg :tool :accessor tool))
  (:default-initargs
    :tool (error "TOOL required.")))

(define-initializer (tool-button setup)
  (setf (q+:checkable tool-button) T)
  (setf (q+:text tool-button) (tool-title tool))
  (setf (q+:tool-tip tool-button) (tool-description tool)))

(define-slot (tool-button changed) ((checked bool))
  (declare (connected tool-button (toggled bool)))
  (let ((tool (tool tool-button)))
    (cond (checked
           (setf (tool *window*) tool))
          (T
           (deselect tool)))))

(define-widget tools-area (QToolBar)
  ((button-map :initform (make-hash-table :test 'eql) :accessor button-map)))

(define-subwidget (tools-area group) (q+:make-qbuttongroup tools-area)
  (setf (q+:exclusive group) T))

(define-subwidget (tools-area gizmo) (make-instance 'tool-options-gizmo)
  (q+:add-dock-widget *window* (q+:qt.right-dock-widget-area) gizmo))

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
  (when (and (eql (c2mop:slot-definition-type slot) 'configurable)
             (slot-boundp *window* 'tools-area))
    (let* ((slot (c2mop:slot-definition-name slot))
           (gizmo (slot-value (slot-value *window* 'tools-area) 'gizmo))
           (layout (slot-value (q+:widget (q+:item-at (slot-value gizmo 'layout) 0)) 'layout))
           (pos (position slot (configurable-slots tool))))
      (let ((widgets (loop for item = (q+:take-at layout 0)
                           until (typep item 'null-qobject)
                           collect (q+:widget item))))
        (when (< pos (length widgets))
          (finalize (nth pos widgets))
          (setf (nth pos widgets) (make-input-for-configurable (slot-value tool slot))))
        (dolist (widget widgets)
          (q+:add-widget layout widget))))))
