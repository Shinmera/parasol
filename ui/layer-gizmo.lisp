#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(with-widget-environment
  (define-widget layer-item (QListWidgetItem)
    ((layer :initarg :layer :initform (error "Layer required.") :accessor layer)))

  (define-initializer widget 100
    (#_setText widget (or (field :name (layer widget)) "?"))))

(with-widget-environment
  (define-widget layer-gizmo (QDockWidget)
    ())

  (define-subwidget central (#_new QWidget widget)
    (#_setWindowTitle widget "Layers")
    (#_setWidget widget central))

  (define-subwidget list (#_new QListWidget widget)
    (#_setMovement list (#_QListView::Snap))
    (#_setDragDropMode list (#_QAbstractItemView::InternalMove)))

  (define-subwidget up (#_new QPushButton "^" widget)
    (#_setMinimumWidth up 50))

  (define-subwidget down (#_new QPushButton "v" widget)
    (#_setMinimumWidth down 50))

  (define-subwidget add (#_new QPushButton "+" widget)
    (#_setMinimumWidth add 50))

  (define-subwidget remove (#_new QPushButton "-" widget)
    (#_setMinimumWidth remove 50))

  (define-subwidget merge (#_new QPushButton "M" widget)
    (#_setMinimumWidth merge 50))

  (define-subwidget copy (#_new QPushButton "C" widget)
    (#_setMinimumWidth copy 50))

  (define-subwidget layout (#_new QGridLayout central)
    (#_setLayout central layout)
    (#_setMargin layout 0)
    (#_setSpacing layout 0)
    (#_setAlignment layout (#_Qt::AlignTop))

    (#_addWidget layout list 0 0 1 6)
    (#_addWidget layout up 1 0 1 1)
    (#_addWidget layout down 1 1 1 1)
    (#_addWidget layout add 1 2 1 1)
    (#_addWidget layout remove 1 3 1 1)
    (#_addWidget layout merge 1 4 1 1)
    (#_addWidget layout copy 1 5 1 1))

  (defun refresh-layers (widget)
    (let ((list (slot-value widget 'list)))
      (#_clear list)
      (loop for i downfrom (1- (size (current-document))) to 0
            for layer = (drawable-at i (current-document))
            do (#_addItem list (make-instance 'layer-item :layer layer)))
      (#_setCurrentRow list (current-index (current-document))))
    (#_repaint (current-view)))

  (define-override drop-event (widget event)
    (declare (ignore event))
    ;; Set new order.
    (loop with size = (size (current-document))
          for source from 0 below size
          for target downfrom (1- size) to 0
          do (setf (aref (drawables (current-document)) target)
                   (layer (#_itemAt widget source))))
    (refresh-layers widget))

  (define-slot select (widget (index int))
    (declare (connected list (current-row-changed int)))
    (declare (ignore index))
    (activate (layer (#_currentItem widget))
              (current-document)))

  (define-slot up (widget)
    (declare (connected up (clicked))))

  (define-slot down (widget)
    (declare (connected down (clicked))))

  (define-slot add (widget)
    (declare (connected add (clicked)))
    (add-layer (current-document))
    (refresh-layers widget))

  (define-slot remove (widget)
    (declare (connected remove (clicked)))
    (extract (current-document) (current-layer (current-document)))
    (refresh-layers widget))

  (define-slot merge (widget)
    (declare (connected merge (clicked)))
    (refresh-layers widget))

  (define-slot copy (widget)
    (declare (connected copy (clicked)))
    (refresh-layers widget))

  (define-initializer widget 100
    (refresh-layers widget)
    (#_setSizePolicy widget (#_QSizePolicy::Minimum) (#_QSizePolicy::Minimum))
    (#_setMinimumWidth widget 200)
    (#_addDockWidget *window* (#_Qt::RightDockWidgetArea) widget)))
