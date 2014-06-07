#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defvar *window*)

(defclass main-window ()
  ((%documents :accessor documents)
   (%current-brush :accessor current-brush)
   (%current-eraser :accessor current-eraser))
  (:metaclass qt-class)
  (:qt-superclass "QMainWindow")
  (:slots ("quit()" mw-quit)))

(defmethod initialize-instance :after ((window main-window) &key)
  (new window)
  (let ((*window* window))
    (#_setWindowTitle window (format NIL "Parasol v~a" (asdf:component-version (asdf:find-system :parasol))))
    (#_resize window 500 500)
    (setf (current-brush window) (make-instance 'brush))

    (let ((documents (#_new QTabWidget))
          (central-splitter (#_new QSplitter (#_Qt::Horizontal)))
          (right-splitter (#_new QSplitter (#_Qt::Vertical))))
      (setf (documents window) documents)

      (#_setHorizontalPolicy (#_sizePolicy documents) (#_QSizePolicy::Expanding))
      (#_setHorizontalPolicy (#_sizePolicy right-splitter) (#_QSizePolicy::Minimum))
      
      (#_addWidget central-splitter documents)
      (#_addWidget central-splitter right-splitter)

      (#_setStretchFactor central-splitter 0 1)
      (#_setStretchFactor central-splitter 1 0)

      (#_setChildrenCollapsible right-splitter NIL)
      (#_addWidget right-splitter (make-instance 'brush-widget))
      (#_addWidget right-splitter (make-instance 'color-widget))
      (#_addWidget right-splitter (make-instance 'layer-widget))
      (#_addWidget right-splitter (#_new QWidget))
      
      (#_setCentralWidget window central-splitter)

      ;; Start off with default doc
      (open-document window))

    ;; Build menu
    (let ((file (#_addMenu (#_menuBar window) "File")))
      (let ((quit (#_new QAction "Quit" window)))
        (connect quit "triggered()" window "quit()")
        (#_addAction file quit)))))

(defmethod open-document ((window main-window) &key (name "Untitled") path)
  (declare (ignore path))
  (#_addTab (documents window) (make-instance 'document :name name) name))

(defun mw-quit (window)
  (#_close window))
