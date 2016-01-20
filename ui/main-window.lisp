#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(defvar *main* NIL)

(define-widget main-window (QMainWindow qui:panel-main-window)
  ())

(define-initializer (main-window setup)
  (setf *main* main-window)
  (setf (q+:window-title main-window) "Parasol"))

(define-finalizer (main-window teardown 100)
  (v:info :parasol "RAPTURE"))

(define-subwidget (main-window toolbar) (q+:make-qtoolbar)
  (q+:add-tool-bar main-window toolbar))

(define-subwidget (main-window view) (make-instance 'document-view)
  (setf (qui:widget :center main-window) view))

(define-subwidget (main-window layers) (make-instance 'layers)
  (qui:add-widget layers main-window))

(define-menu (main-window File)
  (:item ("Open..." (ctrl o)))
  (:item ("Save" (ctrl s)))
  (:item ("Save As..." (ctrl alt s)))
  (:item ("Quit" (ctrl q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information main-window "About Parasol"
                                     "This is still very much a pre-alpha, if even that.")))

(defun current-document ()
  (document (slot-value *main* 'view)))

(defun main ()
  (v:info :parasol "GENESIS")
  (with-main-window (w 'main-window :main-thread NIL)))
