#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defvar *window*)
(defvar *color-history-size* 5)

(defclass main-window ()
  ((%documents-widget :accessor documents-widget)
   (%layer-widget :accessor layer-widget)
   (%brush-widget :accessor brush-widget)
   (%color-widget :accessor color-widget)
   (%repl-widget :accessor repl-widget)
   (%current-brush :accessor current-brush)
   (%current-eraser :accessor current-eraser)
   (%color-history :initform (make-array (1+ *color-history-size*) :initial-element (#_new QColor 0 0 0)) :accessor color-history))
  (:metaclass qt-class)
  (:qt-superclass "QMainWindow")
  (:slots ("quit()" mw-quit)
          ("new()" mw-new)
          ("about()" mw-about)
          ("curve()" mw-curve))
  (:override ("keyReleaseEvent" key-release-event)
             ("mousePressEvent" mouse-press-event)))

(defmethod initialize-instance :after ((window main-window) &key)
  (new window)
  (setf *window* window)
  (#_setWindowTitle window (format NIL "Parasol v~a" (asdf:component-version (asdf:find-system :parasol))))
  (#_resize window 500 500)
  (setf (current-brush window) (make-instance 'brush))
  (setf (current-eraser window) (make-instance 'brush))

  (let ((documents-widget (make-instance 'documents-widget))
        (brush-widget (make-instance 'brush-widget))
        (color-widget (make-instance 'color-widget))
        (layer-widget (make-instance 'layer-widget))
        (repl-widget (make-instance 'repl-widget))
        (central-splitter (#_new QSplitter (#_Qt::Horizontal)))
        (right-splitter (#_new QSplitter (#_Qt::Vertical)))
        (left-splitter (#_new QSplitter (#_Qt::Vertical))))
    (setf (documents-widget window) documents-widget
          (brush-widget window) brush-widget
          (color-widget window) color-widget
          (layer-widget window) layer-widget
          (repl-widget window) repl-widget)

    (#_setHorizontalPolicy (#_sizePolicy documents-widget) (#_QSizePolicy::Expanding))
    (#_setHorizontalPolicy (#_sizePolicy right-splitter) (#_QSizePolicy::Minimum))
    
    (#_addWidget central-splitter left-splitter)
    (#_addWidget central-splitter right-splitter)

    (#_setStretchFactor central-splitter 0 1)
    (#_setStretchFactor central-splitter 1 0)

    (#_setChildrenCollapsible left-splitter NIL)
    (#_addWidget left-splitter documents-widget)
    (#_addWidget left-splitter repl-widget)

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
  (let ((tools (#_addMenu (#_menuBar window) "Tools")))
    (let ((curve (#_new QAction "Curve..." window)))
      (connect curve "triggered()" window "curve()")
      (#_addAction tools curve)))
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

(defun mw-curve (window)
  (declare (ignore window))
  (with-dialog (dialog (make-instance 'curve-dialog))))

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

(defmethod mouse-press-event ((window main-window) event)
  (let ((focused (#_QApplication::focusWidget)))
    (when (qt:qtypep focused (find-qclass "QLineEdit"))
      (#_clearFocus focused)))
  (#_ignore event))

(defmethod key-release-event ((window main-window) event)
  (unless (qt:qtypep (#_QApplication::focusWidget) (find-qclass "QLineEdit"))
    (case (#_key event)
      (90 ;; (#_Qt::Key_Z)
       (undo (current-document window)))
      (89 ;; (#_Qt::Key_Y)
       (redo (current-document window)))
      (32 ;; (#_Qt::Key_Space)
       (#_setFocus (input (repl-widget window))
                   (#_Qt::ShortcutFocusReason))))))

(defmethod color ((window main-window))
  (aref (color-history window) 0))

(defmethod (setf color) (value (window main-window))
  (setf (aref (color-history window) 0) value))

(defmethod push-color ((window main-window) &optional color)
  (let* ((history (color-history window))
         (color (or color (aref history 0))))
    (unless (#_operator== color (aref history 1))
      (loop for i downfrom (1- (length history)) above 0
            do (setf (aref history i)
                     (aref history (1- i))))
      (setf (aref history 0) color))))

(defmethod cycle-color ((window main-window))
  (let* ((history (color-history window))
         (first (aref history 1)))
    (loop for i from 1 below (1- (length history))
          do (setf (aref history i)
                   (aref history (1+ i))))
    (setf (aref history (1- (length history))) first
          (aref history 0) (aref history 1))))

(defmethod current-document ((window main-window))
  (current-document (documents-widget window)))

(defmethod finalize ((window main-window))
  (finalize (documents-widget window))
  (finalize (layer-widget window))
  (finalize (brush-widget window))
  (finalize (color-widget window))
  (finalize (current-brush window))
  (finalize (current-eraser window))
  (loop for color across (color-history window)
        do (maybe-delete-qobject color))
  (setf (documents-widget window) NIL
        (layer-widget window) NIL
        (brush-widget window) NIL
        (color-widget window) NIL
        (current-brush window) NIL
        (current-eraser window) NIL
        (color-history window) NIL))

;; Need to move to resolve class dependency cycle.
;; SHould be in color-widget.lisp
(defmethod push-color :after ((window main-window) &optional color)
  (declare (ignore color))
  (let ((widget (color-widget window)))
    (loop for widget across (color-history widget)
          for color across (color-history *window*)
          do (#_setColor (#_palette widget) (#_QPalette::Background) color)
             (#_update widget))))

(defmethod cycle-color :after ((window main-window))
  (let ((widget (color-widget window)))
    (loop for widget across (color-history widget)
          for color across (color-history *window*)
          do (#_setColor (#_palette widget) (#_QPalette::Background) color)
             (#_update widget))
    (color-widget-update (rgb-widget widget) (color window))
    (color-widget-update (hsv-widget widget) (color window))))
