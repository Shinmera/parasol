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
  (:slots ("quit()" mw-quit)
          ("new()" mw-new)
          ("about()" mw-about)))

(defmethod initialize-instance :after ((window main-window) &key)
  (new window)
  (setf *window* window)
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
    (let ((new (#_new QAction "New" window))
          (quit (#_new QAction "Quit" window)))
      (connect new "triggered()" window "new()")
      (connect quit "triggered()" window "quit()")
      (#_addAction file new)
      (#_addSeparator file)
      (#_addAction file quit)))
  (let ((help (#_addMenu (#_menuBar window) "Help")))
    (let ((about (#_new QAction "About" window)))
      (connect about "triggered()" window "about()")
      (#_addAction help about))))

(defmethod open-document ((window main-window) &key (name "Untitled") path)
  (declare (ignore path))
  (#_addTab (documents window) (make-instance 'document :name name) name))

(defun mw-new (window)
  (open-document window :name (format NIL "Untitled - ~d" (#_count (documents window)))))

(defun mw-quit (window)
  (#_close window))

(defun mw-about (window)
  (let ((parasol (asdf:find-system :parasol)))
    (#_QMessageBox::about window "About Parasol"
                          (format NIL "Parasol v~a<br />~
                                       ~a<br />~
                                       <br />~
                                       Maintainer: ~a<br />~
                                       License: ~a<br />~
                                       <a href=\"https://github.com/Shinmera/parasol\">Parasol on GitHub</a>"
                                  (asdf:component-version parasol)
                                  (asdf:system-description parasol)
                                  (asdf:system-maintainer parasol)
                                  (asdf:system-license parasol)))))
