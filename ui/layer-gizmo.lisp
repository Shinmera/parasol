#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(define-widget layer-item (QWidget)
  ((layer :initarg :layer :initform (error "Layer required.") :accessor layer)))

(define-subwidget (layer-item label) (#_new QLabel)
  (#_setText label (or (field :name layer) "?")))

(define-subwidget (layer-item visible) (#_new QPushButton)
  (#_setSizePolicy visible (#_QSizePolicy::Maximum) (#_QSizePolicy::Maximum))
  (#_setMinimumWidth visible 50)
  (#_setFlat visible T)
  (#_setCheckable visible T))

(define-subwidget (layer-item layout) (#_new QHBoxLayout widget)
  (#_setMargin layout 0)
  (#_setSpacing layout 0)
  (#_addWidget layout visible)
  (#_addWidget layout label)
  (#_setLayout layer-item layout))

(define-widget layer-list (QTableWidget)
  ())

(define-initializer (layer-list setup)
  (#_setDragEnabled layer-list T)
  (#_setAcceptDrops layer-list T)
  (#_setAcceptDrops (#_viewport layer-list) T)
  (#_setDragDropOverwriteMode layer-list NIL)
  (#_setDropIndicatorShown layer-list T)
  (#_setSelectionMode layer-list (#_QAbstractItemView::SingleSelection)) 
  (#_setSelectionBehavior layer-list (#_QAbstractItemView::SelectRows))
  (#_setDragDropMode layer-list (#_QAbstractItemView::InternalMove))
  (#_setStretchLastSection (#_horizontalHeader layer-list) T)
  (#_hide (#_horizontalHeader layer-list))
  (#_hide (#_verticalHeader layer-list))
  (#_setColumnCount layer-list 1))

(defun drop-row (list event)
  (let ((index (#_indexAt list (#_pos event))))
    (or
     (when (and (#_contains (#_rect (#_viewport list)) (#_pos event))
                (#_isValid index))
       (let ((rect (#_visualRect list index))
             (pos (#_pos event)))
         (when (#_contains rect pos)
           (if (< (- (#_bottom rect) (#_y pos)) 2)
               (1+ (#_row index))
               (#_row index)))))
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
  (#_clear list)
  (#_setRowCount list (size (current-document)))
  (loop for i from 0
        for layer across (drawables (current-document))
        do (#_setCellWidget list (- (size (current-document)) i 1) 0 (make-instance 'layer-item :layer layer)))
  (#_selectRow list (- (size (current-document)) 1 (current-index (current-document)))))

(define-override (layer-list drop-event) (event)
  ;; We get to do some manual calculation of drop rows
  ;; because apparently the default behaviour of
  ;; QTableWidget on row dropping is screwed. Goooood.
  (when (and (eql (#_source event) layer-list)
             (not (#_isAccepted event)))
    (when (#_contains (#_rect (#_viewport layer-list)) (#_pos event))
      (move-array-item
       (- (size (current-document)) 1 (#_currentRow layer-list))
       (max 0 (- (size (current-document)) 1 (drop-row layer-list event)))
       (drawables (current-document))))

    (#_accept event)
    (refresh-layers layer-list)))

(define-widget mode-list (QComboBox)
  ((modes :initform '(source-over clear multiply screen overlay darken lighten
                      color-dodge color-burn hard-light soft-light difference
                      exclusion))))

(define-initializer (mode-list setup)
  (#_setToolTip mode "Compositing Mode")
  (dolist (i modes)
    (#_addItem mode (string-downcase i))))

(define-signal (mode-list value-changed) (int))

(define-slot (mode-list new-index) ((value int))
  (declare (connected mode (current-index-changed int)))
  (declare (ignore value))
  (signal! mode value-changed ((value mode) int)))

(defmethod (setf value) (value (mode mode-list))
  (when (numberp value) (setf value (to-mode-name value)))
  (#_setCurrentIndex mode (position value (slot-value mode 'modes)
                                    :test #'string-equal)))

(defmethod value ((mode mode-list))
  (to-mode-num (nth (#_currentIndex mode) (slot-value mode 'modes))))

(define-widget layer-gizmo (QDockWidget)
  ())

(define-subwidget (layer-gizmo central) (#_new QWidget widget)
  (#_setWindowTitle layer-gizmo "Layers")
  (#_setWidget layer-gizmo central))

(define-subwidget (layer-gizmo list) (make-instance 'layer-list))

(define-subwidget (layer-gizmo mode) (make-instance 'mode-list))

(define-subwidget (layer-gizmo opacity) (make-instance 'nice-slider :max 1.0 :step 0.01)
  (#_setToolTip opacity "Opacity"))

(define-subwidget (layer-gizmo up) (#_new QPushButton "^" widget)
  (#_setMinimumWidth up 30))

(define-subwidget (layer-gizmo down) (#_new QPushButton "v" widget)
  (#_setMinimumWidth down 30))

(define-subwidget (layer-gizmo add) (#_new QPushButton "+" widget)
  (#_setMinimumWidth add 30))

(define-subwidget (layer-gizmo remove) (#_new QPushButton "-" widget)
  (#_setMinimumWidth remove 30))

(define-subwidget (layer-gizmo merge) (#_new QPushButton "M" widget)
  (#_setMinimumWidth merge 30))

(define-subwidget (layer-gizmo copy) (#_new QPushButton "C" widget)
  (#_setMinimumWidth copy 30))

(define-subwidget (layer-gizmo layout) (#_new QGridLayout central)
  (#_setLayout central layout)
  (#_setMargin layout 0)
  (#_setSpacing layout 0)
  (#_setAlignment layout (#_Qt::AlignTop))

  (#_addWidget layout list 0 0 1 6)
  (#_addWidget layout mode 1 0 1 6)
  (#_addWidget layout opacity 2 0 1 6)
  (#_addWidget layout up 3 0 1 1)
  (#_addWidget layout down 3 1 1 1)
  (#_addWidget layout add 3 2 1 1)
  (#_addWidget layout remove 3 3 1 1)
  (#_addWidget layout merge 3 4 1 1)
  (#_addWidget layout copy 3 5 1 1))

(defmethod refresh ((widget layer-gizmo))
  (with-slots-bound (widget layer-gizmo)
    (refresh-layers list)
    (let ((layer (current-layer (current-document))))
      (setf (value opacity) (opacity layer))
      (setf (value mode) (mode layer))))
  (#_repaint (current-view)))

(define-slot (layer-gizmo mode) ((selected int))
  (declare (connected mode (value-changed int)))
  (setf (mode (current-layer (current-document))) selected))

(define-slot (layer-gizmo opacity) ((new-opacity double))
  (declare (connected opacity (value-changed double)))
  (setf (opacity (current-layer (current-document))) new-opacity))

(define-slot (layer-gizmo select) ((row int) (column int))
  (declare (connected list (cell-clicked int int)))
  (declare (ignore column))
  (activate (layer (#_cellWidget list row 0)) (current-document))
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
  (#_setSizePolicy layer-gizmo (#_QSizePolicy::Maximum) (#_QSizePolicy::Minimum))
  (#_addDockWidget *window* (#_Qt::RightDockWidgetArea) layer-gizmo))
