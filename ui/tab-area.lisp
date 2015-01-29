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
  (#_setTabText (parent tab) (index tab) text)
  (call-next-method))

(defmethod index ((tab tab))
  (#_indexOf (parent tab) tab))

(defmethod finalize ((tab tab))
  (#_removeTab (parent tab) (index tab))
  (call-next-method))


(define-widget welcome-tab (QWidget tab)
  ((title :initform "Welcome" :accessor title)))

(define-subwidget (welcome-tab label) (#_new QLabel "Welcome to Parasol.")
  (#_setStyleSheet label "background-color:white; color:black;")
  (#_setAlignment label (#_Qt::AlignCenter)))

(define-subwidget (welcome-tab layout) (#_new QHBoxLayout)
  (#_addWidget layout label)
  (#_setLayout welcome-tab layout))


(define-widget tab-area (QTabWidget)
  ())

(defmethod add-tab ((tab-area tab-area) (tab tab))
  (#_addTab tab-area tab (title tab))
  tab)

(defmethod add-tab ((tab-area tab-area) (class symbol))
  (let ((tab (make-instance class :parent tab-area)))
    (#_addTab tab-area tab (title tab))
    tab))

(defmethod change-tab ((tab-area tab-area) (tab tab))
  (#_setCurrentWidget tab-area tab)
  tab)

(define-slot (tab-area change-tab) ((index int))
  (declare (connected tab-area (current-changed int)))
  (declare (method))
  (let ((tab (#_widget tab-area index)))
    (v:info :parasol "Switching to tab ~s" tab)
    tab))

(defmethod close-tab ((tab-area tab-area) (tab tab))
  (finalize tab)
  NIL)

(define-slot (tab-area close-tab) ((index int))
  (declare (connected tab-area (tab-close-requested int)))
  (declare (method))
  (let ((tab (#_widget tab-area index)))
    (v:info :parasol "Removing tab ~s" tab)
    (finalize tab))
  NIL)

(defmethod current-tab ((tab-area tab-area))
  (#_currentWidget tab-area))

(define-initializer (tab-area setup)
  (#_setMovable tab-area T)
  (#_setTabsClosable tab-area T)
  (#_setDocumentMode tab-area T)

  (add-tab tab-area 'document-view))

(define-finalizer (tab-area teardwon)
  (loop for i from 0 below (#_count tab-area)
        for tab = (#_widget tab-area i)
        do (finalize tab))
  (#_clear tab-area))
