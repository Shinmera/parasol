#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defvar *window*)

(defclass main-window ()
  ((%documents-widget :accessor documents-widget)
   (%layer-widget :accessor layer-widget)
   (%brush-widget :accessor brush-widget)
   (%color-widget :accessor color-widget)
   (%current-brush :accessor current-brush)
   (%current-eraser :accessor current-eraser)
   (%color-history :initform (make-array 5 :initial-element (#_new QColor 0 0 0)) :accessor color-history))
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

  (let ((documents-widget (make-instance 'documents-widget))
        (brush-widget (make-instance 'brush-widget))
        (color-widget (make-instance 'color-widget))
        (layer-widget (make-instance 'layer-widget))
        (central-splitter (#_new QSplitter (#_Qt::Horizontal)))
        (right-splitter (#_new QSplitter (#_Qt::Vertical))))
    (setf (documents-widget window) documents-widget
          (brush-widget window) brush-widget
          (color-widget window) color-widget
          (layer-widget window) layer-widget)

    (#_setHorizontalPolicy (#_sizePolicy documents-widget) (#_QSizePolicy::Expanding))
    (#_setHorizontalPolicy (#_sizePolicy right-splitter) (#_QSizePolicy::Minimum))
    
    (#_addWidget central-splitter documents-widget)
    (#_addWidget central-splitter right-splitter)

    (#_setStretchFactor central-splitter 0 1)
    (#_setStretchFactor central-splitter 1 0)

    (#_setChildrenCollapsible right-splitter NIL)
    (#_addWidget right-splitter brush-widget)
    (#_addWidget right-splitter color-widget)
    (#_addWidget right-splitter layer-widget)
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
  (let ((document (make-instance 'document :name name)))
    (#_addTab (documents-widget window) document name)
    (#_setCurrentWidget (documents-widget window) document)))

(defun mw-new (window)
  (open-document window :name (format NIL "Untitled - ~d" (#_count (documents-widget window)))))

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

(defmethod color ((window main-window))
  (aref (color-history window) 0))

(defmethod (setf color) (value (window main-window))
  (let ((history (color-history window)))
    (loop for i downfrom (1- (length history)) above 0
          do (setf (aref history i)
                   (aref history (1- i))))
    (setf (aref history 0) value)))

(defmethod cycle-color ((window main-window))
  (let* ((history (color-history window))
         (first (aref history 0)))
    (loop for i from 0 below (1- (length history))
          do (setf (aref history i)
                   (aref history (1+ i))))
    (setf (aref history (length history)) first)))

(defmethod current-document ((window main-window))
  (current-document (documents-widget window)))
