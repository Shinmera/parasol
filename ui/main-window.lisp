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
    ((tool :initform NIL :accessor tool)))

  (define-initializer window 0
    (unless (boundp '*window*)
      (error "Tried to create a main window without the proper context!"))
    (when *window*
      (error "A main window instance is already active!"))
    (setf *window* window))

  (define-subwidget tab-area (make-instance 'tab-area))

  ;; This should be automated / moved out to the individual
  ;; gizmos...
  (define-subwidget tools-area (make-instance 'tools-area))

  (define-subwidget layer-gizmo (make-instance 'layer-gizmo))

  (define-subwidget history-gizmo (make-instance 'history-gizmo))

  (define-subwidget layout-container (#_new QWidget)
    (let ((layout (#_new QVBoxLayout layout-container)))
      (#_setMargin layout 0)
      (#_setSpacing layout 0)
      (#_addWidget layout tools-area)
      (#_addWidget layout tab-area))
    (#_setCentralWidget widget layout-container))

  (define-initializer window 100
    (#_setWindowTitle window (format NIL "Parasol v~a" (asdf:component-version (asdf:find-system :parasol))))
    (#_setTabPosition window (#_Qt::AllDockWidgetAreas) (#_QTabWidget::North))
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
  (v:info :parasol "GENESIS")
  (let* ((*window*)
         (window (make-instance 'main-window)))
    (#_show window)
    (#_exec *qapplication*)
    (v:info :parasol "RAPTURE")
    (finalize window)
    (trivial-garbage:gc :full T)
    NIL))
