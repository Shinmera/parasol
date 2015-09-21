#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric index (tab))
  (defgeneric add-tab (tab-area tab))
  (defgeneric change-tab (tab-area tab))
  (defgeneric close-tab (tab-area tab))
  (defgeneric current-tab (tab-area)))

(define-widget tab (QWidget)
  ((title :initarg :title :initform "Untitled" :accessor title)
   (parent :initarg :parent :initform (error "Parent required.") :accessor parent)))

(defmethod (setf title) ((text string) (tab tab))
  (setf (q+:tab-text (parent tab) (index tab)) text)
  (call-next-method))

(defmethod index ((tab tab))
  (q+:index-of (parent tab) tab))

(defmethod finalize ((tab tab))
  (q+:remove-tab (parent tab) (index tab))
  (call-next-method))


(define-widget welcome-tab (QWidget tab)
  ((title :initform "Welcome" :accessor title)))

(define-subwidget (welcome-tab label) (q+:make-qlabel "Welcome to Parasol.")
  (setf (q+:style-sheet label) "background-color:white; color:black;")
  (setf (q+:alignment label) (q+:qt.align-center)))

(define-subwidget (welcome-tab layout) (q+:make-qhboxlayout)
  (q+:add-widget layout label)
  (setf (q+:layout welcome-tab) layout))


(define-widget tab-area (QTabWidget)
  ())

(defmethod add-tab ((tab-area tab-area) (tab tab))
  (q+:add-tab tab-area tab (title tab))
  tab)

(defmethod add-tab ((tab-area tab-area) (class symbol))
  (let ((tab (make-instance class :parent tab-area)))
    (q+:add-tab tab-area tab (title tab))
    tab))

(defmethod change-tab ((tab-area tab-area) (tab tab))
  (setf (q+:current-widget tab-area) tab)
  tab)

(define-slot (tab-area change-tab) ((index int))
  (declare (connected tab-area (current-changed int)))
  (declare (method))
  (let ((tab (q+:widget tab-area index)))
    (v:info :parasol "Switching to tab ~s" tab)
    tab))

(defmethod close-tab ((tab-area tab-area) (tab tab))
  (finalize tab)
  NIL)

(define-slot (tab-area close-tab) ((index int))
  (declare (connected tab-area (tab-close-requested int)))
  (declare (method))
  (let ((tab (q+:widget tab-area index)))
    (v:info :parasol "Removing tab ~s" tab)
    (finalize tab))
  NIL)

(defmethod current-tab ((tab-area tab-area))
  (q+:current-widget tab-area))

(define-initializer (tab-area setup)
  (setf (q+:movable tab-area) T)
  (setf (q+:tabs-closable tab-area) T)
  (setf (q+:document-mode tab-area) T)

  (add-tab tab-area 'document-view))

(define-finalizer (tab-area teardwon)
  (loop for i from 0 below (q+:count tab-area)
        for tab = (q+:widget tab-area i)
        do (finalize tab))
  (q+:clear tab-area))
