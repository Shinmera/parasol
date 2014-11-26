#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(with-widget-environment
  (define-widget layer-item (QWidget)
    ((layer :initarg :layer :initform (error "Layer required.") :accessor layer)))

  (define-subwidget label (#_new QLabel)
    (#_setText label (or (field :name layer) "?")))

  (define-subwidget visible (#_new QPushButton)
    (#_setSizePolicy visible (#_QSizePolicy::Maximum) (#_QSizePolicy::Maximum))
    (#_setMinimumWidth visible 50)
    (#_setFlat visible T)
    (#_setCheckable visible T))

  (define-layout layout (#_new QHBoxLayout widget)
    (#_setMargin layout 0)
    (#_setSpacing layout 0)
    (#_addWidget layout visible)
    (#_addWidget layout label)))

(with-widget-environment
  (define-widget layer-list (QTableWidget)
    ())

  (define-initializer list 100
    (#_setDragEnabled list T)
    (#_setAcceptDrops list T)
    (#_setAcceptDrops (#_viewport list) T)
    (#_setDragDropOverwriteMode list NIL)
    (#_setDropIndicatorShown list T)
    (#_setSelectionMode list (#_QAbstractItemView::SingleSelection)) 
    (#_setSelectionBehavior list (#_QAbstractItemView::SelectRows))
    (#_setDragDropMode list (#_QAbstractItemView::InternalMove))
    (#_setStretchLastSection (#_horizontalHeader list) T)
    (#_hide (#_horizontalHeader list))
    (#_hide (#_verticalHeader list))
    (#_setColumnCount list 1))

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

  (define-override drop-event (list event)
    ;; We get to do some manual calculation of drop rows
    ;; because apparently the default behaviour of
    ;; QTableWidget on row dropping is screwed. Goooood.
    (when (and (eql (#_source event) list)
               (not (#_isAccepted event)))
      (when (#_contains (#_rect (#_viewport list)) (#_pos event))
        (move-array-item
         (- (size (current-document)) 1 (#_currentRow list))
         (max 0 (- (size (current-document)) 1 (drop-row list event)))
         (drawables (current-document))))

      (#_accept event)
      (refresh-layers list))))

(with-widget-environment
  (define-widget mode-list (QComboBox)
    ((modes :initform '(source-over clear multiply screen overlay darken lighten
                        color-dodge color-burn hard-light soft-light difference
                        exclusion))))

  (define-initializer mode 100
    (#_setToolTip mode "Compositing Mode")
    (dolist (i modes)
      (#_addItem mode (string-downcase i))))

  (define-signal value-changed (int))

  (define-slot new-index (mode (value int))
    (declare (connected mode (current-index-changed int)))
    (declare (ignore value))
    (signal! mode value-changed ((value mode) int)))

  (defmethod (setf value) (value (mode mode-list))
    (when (numberp value) (setf value (to-mode-name value)))
    (#_setCurrentIndex mode (position value (slot-value mode 'modes)
                                      :test #'string-equal)))

  (defmethod value ((mode mode-list))
    (to-mode-num (nth (#_currentIndex mode) (slot-value mode 'modes)))))

(with-widget-environment
  (define-widget layer-gizmo (QDockWidget)
    ())

  (define-subwidget central (#_new QWidget widget)
    (#_setWindowTitle widget "Layers")
    (#_setWidget widget central))

  (define-subwidget list (make-instance 'layer-list))

  (define-subwidget mode (make-instance 'mode-list))

  (define-subwidget opacity (make-instance 'nice-slider :max 1.0 :step 0.01)
    (#_setToolTip opacity "Opacity"))

  (define-subwidget up (#_new QPushButton "^" widget)
    (#_setMinimumWidth up 30))

  (define-subwidget down (#_new QPushButton "v" widget)
    (#_setMinimumWidth down 30))

  (define-subwidget add (#_new QPushButton "+" widget)
    (#_setMinimumWidth add 30))

  (define-subwidget remove (#_new QPushButton "-" widget)
    (#_setMinimumWidth remove 30))

  (define-subwidget merge (#_new QPushButton "M" widget)
    (#_setMinimumWidth merge 30))

  (define-subwidget copy (#_new QPushButton "C" widget)
    (#_setMinimumWidth copy 30))

  (define-subwidget layout (#_new QGridLayout central)
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

  (define-slot mode (widget (selected int))
    (declare (connected mode (value-changed int)))
    (setf (mode (current-layer (current-document))) selected))

  (define-slot opacity (widget (new-opacity double))
    (declare (connected opacity (value-changed double)))
    (setf (opacity (current-layer (current-document))) new-opacity))

  (define-slot select (widget (row int) (column int))
    (declare (connected list (cell-clicked int int)))
    (declare (ignore column))
    (activate (layer (#_cellWidget list row 0)) (current-document))
    (refresh widget))

  ;; FIXME: Hook into history system
  (define-slot up (widget)
    (declare (connected up (clicked)))
    ;; TODO
    (refresh widget))

  (define-slot down (widget)
    (declare (connected down (clicked)))
    ;; TODO
    (refresh widget))

  (define-slot add (widget)
    (declare (connected add (clicked)))
    (add-layer (current-document))
    (refresh widget))

  (define-slot remove (widget)
    (declare (connected remove (clicked)))
    (extract (current-document) (current-layer (current-document)))
    (refresh widget))

  (define-slot merge (widget)
    (declare (connected merge (clicked)))
    ;; TODO
    (refresh widget))

  (define-slot copy (widget)
    (declare (connected copy (clicked)))
    ;; TODO
    (refresh widget))

  (define-initializer widget 100
    (refresh widget)
    (#_setSizePolicy widget (#_QSizePolicy::Maximum) (#_QSizePolicy::Minimum))
    (#_addDockWidget *window* (#_Qt::RightDockWidgetArea) widget)))
