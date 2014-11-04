#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(named-readtables:in-readtable :qtools)

(defvar *window*)

(with-widget-environment
  (define-widget main-window ("QMainWindow")
    ())

  (define-subwidget tab-area (make-instance 'tab-area))
  (define-subwidget gizmo-bar (make-instance 'gizmo-bar))
  (define-subwidget central-splitter (#_new QSplitter (#_Qt::Horizontal))
    (#_setCentralWidget widget central-splitter))

  (define-initializer window 100
    (unless (boundp '*window*)
      (error "Tried to create a main window without the proper context!"))
    (when *window*
      (error "A main window instance is already active!"))
    (setf *window* window)

    (#_setWindowTitle window (format NIL "Parasol v~a" (asdf:component-version (asdf:find-system :parasol))))
    (#_resize window 500 500)

    (#_addWidget central-splitter tab-area)
    (#_addWidget central-splitter gizmo-bar)

    (#_setStretchFactor central-splitter 0 1)
    (#_setStretchFactor central-splitter 1 0)))

(defun main ()
  (let ((*window*))
    (with-main-window (window (make-instance 'main-window)))))
