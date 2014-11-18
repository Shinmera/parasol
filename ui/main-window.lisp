#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(defvar *window*)

(with-widget-environment
  (define-widget main-window (QMainWindow)
    ((tool :initform (make-instance 'brush-tool) :accessor tool)))

  (define-subwidget tab-area (make-instance 'tab-area))

  (define-subwidget gizmo-bar (make-instance 'gizmo-bar))

  (define-subwidget central-splitter (#_new QSplitter (#_Qt::Horizontal))
    (#_setCentralWidget widget central-splitter)

    (#_addWidget central-splitter tab-area)
    (#_addWidget central-splitter gizmo-bar)

    (#_setStretchFactor central-splitter 0 1)
    (#_setStretchFactor central-splitter 1 0))

  (define-initializer window 100
    (unless (boundp '*window*)
      (error "Tried to create a main window without the proper context!"))
    (when *window*
      (error "A main window instance is already active!"))
    (setf *window* window)

    (#_setWindowTitle window (format NIL "Parasol v~a" (asdf:component-version (asdf:find-system :parasol))))
    (#_resize window 500 500))

  (define-menu File
    (:item (New (ctrl n))
      (change-tab tab-area (add-tab tab-area 'document-view)))
    (:item (Load (ctrl l)))
    (:item (Save (ctrl s)))
    (:item (Save-As (ctrl alt s)))
    (:separator)
    (:item (Quit (ctrl q))
      (#_close widget)))

  (define-menu Edit
    (:separator)
    (:item Keychords
      (#_exec (make-widget 'qtools:keychord-editor (widget))))
    (:item Settings))

  (define-menu Help
    (:item About
      (let ((system (asdf:find-system :parasol)))
        (with-finalizing ((box (#_new QMessageBox widget)))
          (#_setText box (format NIL "~a<br />
The source code is openly available and licensed under ~a.<br />
<br />
Homepage: <a href=\"~a~:*\">~a</a><br />
Author: ~a<br />
Version: ~a"
                                 (asdf:system-description system)
                                 (asdf:system-license system)
                                 (asdf:system-homepage system)
                                 (asdf:system-author system)
                                 (asdf:component-version system)))
          (#_exec box))))))

(defun current-view ()
  (let ((tab (current-tab (slot-value *window* 'tab-area))))
    (when (typep tab 'document-view)
      tab)))

(defun current-document ()
  (let ((view (current-view)))
    (when view
      (document view))))

(defun main ()
  (let ((*window*))
    (with-main-window (window (make-instance 'main-window)))))
