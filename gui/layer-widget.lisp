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
  ((%list-widget :accessor list-widget)
   (%opacity :accessor opacity)
   (%mode :accessor mode))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("addLayer()" add-layer)
          ("removeLayer()" remove-layer)
          ("activateLayer(int)" activate-layer)
          ("changeMode(const QString)" change-mode)
          ("changeOpacity(double)" change-opacity)))

(defmethod initialize-instance :after ((widget layer-widget) &key)
  (new widget)
  (let ((layout (#_new QGridLayout))
        (list (make-instance 'layer-list-widget))
        (button-add (#_new QPushButton "+"))
        (button-remove (#_new QPushButton "-"))
        (mode (#_new QComboBox))
        (opacity (make-instance 'ex-slider-widget :max 1.0 :step 0.1 :divisor 100)))
    (setf (list-widget widget) list
          (opacity widget) opacity
          (mode widget) mode)
    (#_setDragDropMode list (#_QAbstractItemView::InternalMove))
    (#_setSelectionMode list (#_QAbstractItemView::SingleSelection))
    (loop for key being the hash-keys of *compositing-mode-map*
          do (#_addItem mode key))
    (#_addWidget layout (#_new QLabel "Layers") 0 0 1 2)
    (#_addWidget layout list 1 0 1 2)
    (#_addWidget layout opacity 2 0 1 2)
    (#_addWidget layout mode 3 0 1 2)
    (#_addWidget layout button-add 4 0)
    (#_addWidget layout button-remove 4 1)
    (#_setLayout widget layout)
    (connect button-add "clicked()" widget "addLayer()")
    (connect button-remove "clicked()" widget "removeLayer()")
    (connect list "currentRowChanged(int)" widget "activateLayer(int)")
    (connect opacity "valueChanged(double)" widget "changeOpacity(double)")
    (connect mode "currentIndexChanged(const QString)" widget "changeMode(const QString)")))

(defun update-layer-widget ()
  (with-objects ((size (#_new QSize 10 30)))
    (let* ((widget (layer-widget *window*))
           (list (list-widget widget))
           (document (current-document *window*)))
      (#_clear list)
      ;; insert reverse
      (loop for i downfrom (1- (length (layers document))) to 0
            for layer = (aref (layers document) i)
            do (let ((item (#_new QListWidgetItem (name layer))))
                 (#_setSizeHint item size)
                 (#_addItem list item)))
      (#_setCurrentRow list (min (- (length (layers document))
                                    (active-layer-index document))
                                 (1- (length (layers document)))))
      (setf (value (opacity widget)) (opacity (active-layer document)))
      (#_setCurrentIndex (mode widget) (position (mode (active-layer document))
                                                 *compositing-mode-list*
                                                 :key #'second)))))

;; Hook into document change
(defmethod make-active :after ((document document))
  (update-layer-widget))

(defmethod add-layer ((widget layer-widget) &key name)
  (declare (ignore name mode))
  (add-layer (current-document *window*))
  (update-layer-widget))

(defmethod remove-layer ((widget layer-widget) &optional index)
  (declare (ignore index))
  (let ((index (#_currentRow (list-widget widget))))
    (remove-layer (current-document *window*)
                  (- (length (layers (current-document *window*))) 1 index))
    (update-layer-widget)
    (if (< index (#_count (list-widget widget)))
        (#_setCurrentRow (list-widget widget) index)
        (#_setCurrentRow (list-widget widget) (1- index)))))

(defmethod activate-layer ((widget layer-widget) index)
  (unless (< index 0)
    (let ((document (current-document *window*)))
      ;; Reverse index again
      (setf (active-layer document)
            (- (length (layers document)) 1 index))
      (setf (value (opacity widget)) (opacity (active-layer document)))
      (#_setCurrentIndex (mode widget) (position (mode (active-layer document))
                                                 *compositing-mode-list*
                                                 :key #'second)))))

(defmethod change-opacity ((widget layer-widget) opacity)
  (setf (opacity (active-layer (current-document *window*))) opacity)
  (#_update (current-document *window*)))

(defmethod change-mode ((widget layer-widget) mode-name)
  (let ((mode-int (gethash mode-name *compositing-mode-map*)))
    (setf (mode (active-layer (current-document *window*))) mode-int))
  (#_update (current-document *window*)))

(defmethod drop-event ((widget layer-list-widget) event)
  ;; We can't call the qt-superclass' method here, unfortunately.
  ;; Reimplementing the drop is done lazily by just changing the order
  ;; and then rebuilding it completely. Why not.
  (let ((insert-row (#_row widget (#_itemAt widget (#_pos event)))))
    (if (< insert-row 0)
        (move-layer (current-document *window*) 0)
        (move-layer (current-document *window*)
                    (- (length (layers (current-document *window*))) 1 insert-row)))
    (update-layer-widget)
    (#_setCurrentRow widget insert-row)))

(defmethod finalize ((widget layer-widget))
  (cleanup (widget) list-widget mode opacity))
