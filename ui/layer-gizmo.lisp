#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(define-widget layer-item (QWidget)
  ((layer :initarg :layer :initform (error "Layer required.") :accessor layer)))

(define-subwidget (layer-item label) (q+:make-qlabel)
  (setf (q+:text label) (or (field :name layer) "?")))

(define-subwidget (layer-item visible) (q+:make-qpushbutton)
  (setf (q+:size-policy visible) (q+:qsizepolicy.maximum) (q+:qsizepolicy.maximum))
  (setf (q+:minimum-width visible) 50)
  (setf (q+:flat visible) T)
  (setf (q+:checkable visible) T))

(define-subwidget (layer-item layout) (q+:make-qhboxlayout layer-item)
  (setf (q+:,argin layout) 0)
  (setf (q+:spacing layout) 0)
  (q+:add-widget layout visible)
  (q+:add-widget layout label)
  (setf (q+:layout layer-item) layout))

(define-widget layer-list (QTableWidget)
  ())

(define-initializer (layer-list setup)
  (setf (q+:drag-enabled layer-list) T)
  (setf (q+:accept-drops layer-list) T)
  (setf (q+:accept-drops (q+:viewport layer-list)) T)
  (setf (q+:drag-drop-overwrite-mode layer-list) NIL)
  (setf (q+:drop-indicator-shown layer-list) T)
  (setf (q+:selection-mode layer-list) (q+:qabstractitemview.single-selection)) 
  (setf (q+:selection-behavior layer-list) (q+:qabstractitemview.select-rows))
  (setf (q+:drag-drop-mode layer-list) (q+:qabstractitemview.internal-move))
  (setf (q+:stretch-last-section (q+:horizontal-header layer-list)) T)
  (q+:hide (q+:horizontal-header layer-list))
  (q+:hide (q+:vertical-header layer-list))
  (setf (q+:column-count layer-list) 1))

(defun drop-row (list event)
  (let ((index (q+:index-at list (q+:pos event))))
    (or
     (when (and (q+:contains (q+:rect (q+:viewport list)) (q+:pos event))
                (q+:is-valid index))
       (let ((rect (q+:visual-rect list index))
             (pos (q+:pos event)))
         (when (q+:contains rect pos)
           (if (< (- (q+:bottom rect) (q+:y pos)) 2)
               (1+ (q+:row index))
               (q+:row index)))))
     0)))

(defun move-array-item (from to array)
  (let ((item (aref array from))
        (size (length array)))
    (loop for i from from below (1- size)
          do (setf (aref array i)
                   (aref array (1+ i))))
    (loop for i downfrom (1- size) above to
          do (setf (aref array i)
                   (aref array (1- i))))
    (setf (aref array to) item))
  array)

(defun refresh-layers (list)
  (q+:clear list)
  (setf (q+:row-count list) (size (current-document)))
  (loop for i from 0
        for layer across (drawables (current-document))
        do (setf (q+:cell-widget list) (values (- (size (current-document)) i 1) 0 (make-instance 'layer-item :layer layer))))
  (q+:select-row list (- (size (current-document)) 1 (current-index (current-document)))))

(define-override (layer-list drop-event) (event)
  ;; We get to do some manual calculation of drop rows
  ;; because apparently the default behaviour of
  ;; QTableWidget on row dropping is screwed. Goooood.
  (when (and (eql (q+:source event) layer-list)
             (not (q+:is-accepted event)))
    (when (q+:contains (q+:rect (q+:viewport layer-list)) (q+:pos event))
      (move-array-item
       (- (size (current-document)) 1 (q+:current-row layer-list))
       (max 0 (- (size (current-document)) 1 (drop-row layer-list event)))
       (drawables (current-document))))

    (q+:accept event)
    (refresh-layers layer-list)))

(define-widget mode-list (QComboBox)
  ((modes :initform '(source-over clear multiply screen overlay darken lighten
                      color-dodge color-burn hard-light soft-light difference
                      exclusion))))

(define-initializer (mode-list setup)
  (setf (q+:tool-tip mode-list) "Compositing Mode")
  (dolist (i modes)
    (q+:add-item mode-list (string-downcase i))))

(define-signal (mode-list value-changed) (int))

(define-slot (mode-list new-index) ((value int))
  (declare (connected mode-list (current-index-changed int)))
  (declare (ignore value))
  (signal! mode-list (value-changed int) (value mode-list)))

(defmethod (setf value) (value (mode-list mode-list))
  (when (numberp value) (setf value (to-mode-name value)))
  (setf (q+:current-index mode-list) (position value (slot-value mode-list 'modes)
                                               :test #'string-equal)))

(defmethod value ((mode-list mode-list))
  (to-mode-num (nth (q+:current-index mode-list)
                    (slot-value mode-list 'modes))))

(define-widget layer-gizmo (QDockWidget)
  ())

(define-subwidget (layer-gizmo central) (q+:make-qWidget layer-gizmo)
  (setf (q+:window-title layer-gizmo) "Layers")
  (setf (q+:widget layer-gizmo) central))

(define-subwidget (layer-gizmo list) (make-instance 'layer-list))

(define-subwidget (layer-gizmo mode) (make-instance 'mode-list))

(define-subwidget (layer-gizmo opacity) (make-instance 'nice-slider :max 1.0 :step 0.01)
  (setf (q+:tool-tip opacity) "Opacity"))

(define-subwidget (layer-gizmo up) (q+:make-qPushButton "^" layer-gizmo)
  (setf (q+:minimum-width up) 30))

(define-subwidget (layer-gizmo down) (q+:make-qPushButton "v" layer-gizmo)
  (setf (q+:minimum-width down) 30))

(define-subwidget (layer-gizmo add) (q+:make-qPushButton "+" layer-gizmo)
  (setf (q+:minimum-width add) 30))

(define-subwidget (layer-gizmo remove) (q+:make-qPushButton "-" layer-gizmo)
  (setf (q+:minimum-width remove) 30))

(define-subwidget (layer-gizmo merge) (q+:make-qPushButton "M" layer-gizmo)
  (setf (q+:minimum-width merge) 30))

(define-subwidget (layer-gizmo copy) (q+:make-qPushButton "C" layer-gizmo)
  (setf (q+:minimum-width copy) 30))

(define-subwidget (layer-gizmo layout) (q+:make-qGridLayout central)
  (setf (q+:layout central) layout)
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 0)
  (setf (q+:alignment layout) (q+:qt.align-top))

  (q+:add-widget layout list 0 0 1 6)
  (q+:add-widget layout mode 1 0 1 6)
  (q+:add-widget layout opacity 2 0 1 6)
  (q+:add-widget layout up 3 0 1 1)
  (q+:add-widget layout down 3 1 1 1)
  (q+:add-widget layout add 3 2 1 1)
  (q+:add-widget layout remove 3 3 1 1)
  (q+:add-widget layout merge 3 4 1 1)
  (q+:add-widget layout copy 3 5 1 1))

(defmethod refresh ((layer-gizmo layer-gizmo))
  (with-slots-bound (layer-gizmo layer-gizmo)
    (refresh-layers list)
    (let ((layer (current-layer (current-document))))
      (setf (value opacity) (opacity layer))
      (setf (value mode) (mode layer))))
  (q+:repaint (current-view)))

(define-slot (layer-gizmo mode) ((selected int))
  (declare (connected mode (value-changed int)))
  (setf (mode (current-layer (current-document))) selected))

(define-slot (layer-gizmo opacity) ((new-opacity double))
  (declare (connected opacity (value-changed double)))
  (setf (opacity (current-layer (current-document))) new-opacity))

(define-slot (layer-gizmo select) ((row int) (column int))
  (declare (connected list (cell-clicked int int)))
  (declare (ignore column))
  (activate (layer (q+:cell-widget list row 0)) (current-document))
  (refresh layer-gizmo))

;; FIXME: Hook into history system
(define-slot (layer-gizmo up) ()
  (declare (connected up (clicked)))
  ;; TODO
  (refresh layer-gizmo))

(define-slot (layer-gizmo down) ()
  (declare (connected down (clicked)))
  ;; TODO
  (refresh layer-gizmo))

(define-slot (layer-gizmo add) ()
  (declare (connected add (clicked)))
  (add-layer (current-document))
  (refresh layer-gizmo))

(define-slot (layer-gizmo remove) ()
  (declare (connected remove (clicked)))
  (extract (current-document) (current-layer (current-document)))
  (refresh layer-gizmo))

(define-slot (layer-gizmo merge) ()
  (declare (connected merge (clicked)))
  ;; TODO
  (refresh layer-gizmo))

(define-slot (layer-gizmo copy) ()
  (declare (connected copy (clicked)))
  ;; TODO
  (refresh layer-gizmo))

(define-initializer (layer-gizmo setup)
  (refresh layer-gizmo)
  (setf (q+:size-policy layer-gizmo) (q+:qsizepolicy.maximum) (q+:qsizepolicy.minimum)))
  (q+:add-dock-widget *window* (q+:qt.right-dock-widget-area) layer-gizmo))
