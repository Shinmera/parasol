#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass layer-list-widget ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QListWidget")
  (:override ("dropEvent" drop-event)))

(defmethod initialize-instance :after ((widget layer-list-widget) &key)
  (new widget))

(defclass layer-widget ()
  ((%list-widget :accessor list-widget))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("addLayer()" add-layer)
          ("removeLayer()" remove-layer)
          ("activateLayer(int)" activate-layer)))

(defmethod initialize-instance :after ((widget layer-widget) &key)
  (new widget)
  (let ((layout (#_new QGridLayout))
        (list (make-instance 'layer-list-widget))
        (button-add (#_new QPushButton "+"))
        (button-remove (#_new QPushButton "-")))
    (#_setDragDropMode list (#_QAbstractItemView::InternalMove))
    (#_setSelectionMode list (#_QAbstractItemView::SingleSelection))
    (#_addWidget layout (#_new QLabel "Layers") 0 0 1 2)
    (#_addWidget layout list 1 0 1 2)
    (#_addWidget layout button-add 2 0)
    (#_addWidget layout button-remove 2 1)
    (#_setLayout widget layout)
    (connect button-add "clicked()" widget "addLayer()")
    (connect button-remove "clicked()" widget "removeLayer()")
    (connect list "currentRowChanged(int)" widget "activateLayer(int)")
    (setf (list-widget widget) list)))

(defun update-layer-widget ()
  (let ((widget (list-widget (layer-widget *window*)))
        (document (current-document *window*)))
    (#_clear widget)
    ;; insert reverse
    (loop for i downfrom (1- (length (layers document))) to 0
          for layer = (aref (layers document) i)
          do (#_addItem widget (name layer)))
    (when (< (#_currentRow widget) 0)
      (#_setCurrentRow widget 0))))

;; Hook into document change
(defmethod make-active :after ((document document))
  (update-layer-widget))

(defmethod add-layer ((widget layer-widget) &key name mode)
  (declare (ignore name mode))
  (add-layer (current-document *window*))
  (update-layer-widget))

(defmethod remove-layer ((widget layer-widget) &optional index)
  (declare (ignore index))
  (let ((index (#_currentRow (list-widget widget))))
    (remove-layer (current-document *window*) index)
    (update-layer-widget)
    (if (< index (#_count (list-widget widget)))
        (#_setCurrentRow (list-widget widget) index)
        (#_setCurrentRow (list-widget widget) (1- index)))))

(defmethod activate-layer ((widget layer-widget) index)
  (unless (< index 0)
    ;; Reverse index again
    (setf (active-layer (current-document *window*))
          (- (length (layers (current-document *window*))) 1 index))))

(defmethod drop-event ((widget layer-list-widget) event)
  ;; We can't call the qt-superclass' method here, unfortunately.
  ;; Reimplementing the drop is done lazily by just changing the order
  ;; and then rebuilding it completely. Why not.
  (let ((insert-row (#_row widget (#_itemAt widget (#_pos event)))))
    (move-layer (current-document *window*)
                (- (length (layers (current-document *window*))) 1 insert-row))
    (update-layer-widget)
    (#_setCurrentRow widget insert-row)))
