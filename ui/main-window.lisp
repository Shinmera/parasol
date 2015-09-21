#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(defvar *window*)

(define-widget main-window (QMainWindow)
  ((tool :initform NIL :accessor tool)))

(define-initializer (main-window init-window-system 100)
  (unless (boundp '*window*)
    (error "Tried to create a main window without the proper context!"))
  (when *window*
    (error "A main window instance is already active!"))
  (setf *window* main-window))

(define-subwidget (main-window tab-area) (make-instance 'tab-area))

;; This should be automated / moved out to the individual
;; gizmos...
(define-subwidget (main-window tools-area) (make-instance 'tools-area))

(define-subwidget (main-window layer-gizmo) (make-instance 'layer-gizmo))

(define-subwidget (main-window history-gizmo) (make-instance 'history-gizmo))

(define-subwidget (main-window layout-container) (q+:make-qwidget)
  (let ((layout (q+:make-qvboxlayout layout-container)))
    (setf (q+:margin layout) 0)
    (setf (q+:spacing layout) 0)
    (q+:add-widget layout tab-area))
  (setf (q+:central-widget main-window) layout-container))

(define-initializer (main-window setup)
  (setf (q+:window-title main-window) (format NIL "Parasol v~a" (asdf:component-version (asdf:find-system :parasol))))
  (setf (q+:tab-position main-window (q+:qt.all-dock-widget-areas)) (q+:qtabwidget.north))
  (q+:resize main-window 500 500))

(define-menu (main-window File)
  (:item ("New" (ctrl n))
    (change-tab tab-area (add-tab tab-area 'document-view)))
  (:item ("Load" (ctrl l)))
  (:item ("Save" (ctrl s)))
  (:item ("Save As..." (ctrl alt s)))
  (:separator)
  (:item ("Quit" (ctrl q))
    (q+:close main-window)))

(define-menu (main-window Edit)
  (:separator)
  (:item "Keychords"
         ;; (q+:exec (make-widget 'qtools:keychord-editor (widget)))
         )
  (:item "Settings"))

(define-menu (main-window Help)
  (:item "About"
    (let ((system (asdf:find-system :parasol)))
      (with-finalizing ((box (q+:make-qmessagebox main-window)))
        (setf (q+:text box) (format NIL "~a<br />
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
        (q+:exec box)))))

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
    (q+:show window)
    (q+:exec *qapplication*)
    (v:info :parasol "RAPTURE")
    (finalize window)
    (trivial-garbage:gc :full T)
    NIL))
