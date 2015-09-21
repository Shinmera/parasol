#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(define-widget history-list (QTableWidget)
  ())

(define-initializer (history-list setup)
  (setf (q+:selection-mode history-list) (q+:qabstractitemview.single-selection)) 
  (setf (q+:selection-behavior history-list) (q+:qabstractitemview.select-rows))
  (setf (q+:stretch-last-section (q+:horizontal-header history-list)) T)
  (q+:hide (q+:horizontal-header history-list))
  (q+:hide (q+:vertical-header history-list))
  (setf (q+:column-count history-list) 1))

(define-widget history-gizmo (QDockWidget)
  ())

(define-subwidget (history-gizmo central) (q+:make-qwidget history-gizmo)
  (setf (q+:window-title history-gizmo) "History")
  (setf (q+:widget history-gizmo) central))

(define-subwidget (history-gizmo list) (make-instance 'history-list))

(define-subwidget (history-gizmo layout) (q+:make-qgridlayout central)
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 0)
  (setf (q+:alignment layout) (q+:qt.align-top)))

(define-initializer (history-gizmo setup)
  (setf (q+:size-policy history-gizmo) (q+:qsizepolicy-maximum) (q+:qsizepolicy-minimum))
  (q+:add-cock-widget *window* (q+:qt.right-dock-widget-area) history-gizmo)
  (q+:tabify-dock-widget *window* history-gizmo (slot-value *window* 'layer-gizmo)))
