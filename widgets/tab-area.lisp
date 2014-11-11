#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(titled-readtables:in-readtable :qtools)

(with-widget-environment
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
    (call-next-method)))

(with-widget-environment
  (define-widget welcome-tab (QWidget tab)
    ((title :initform "Welcome" :accessor title)))

  (define-subwidget label (#_new QLabel "Welcome to Parasol.")
    (#_setStyleSheet label "background-color:white; color:black;")
    (#_setAlignment label (#_Qt::AlignCenter)))

  (define-layout layout (#_new QHBoxLayout)
    (#_addWidget layout label)))

(with-widget-environment
  (define-widget tab-area (QTabWidget)
    ())

  (define-subwidget welcome (make-instance 'welcome-tab :parent widget))

  (defmethod add-tab ((widget tab-area) (tab tab))
    (#_addTab widget tab (title tab))
    tab)

  (defmethod add-tab ((widget tab-area) (class symbol))
    (let ((tab (make-instance class :parent widget)))
      (#_addTab widget tab (title tab))
      tab))

  (define-slot change-tab (widget (index int))
    (declare (connected widget (current-changed int)))
    (declare (method))
    (let ((tab (#_widget widget index)))
      (v:info :parasol "Switching to tab ~s" tab)
      tab))

  (defmethod change-tab ((widget tab-area) (tab tab))
    (#_setCurrentWidget widget tab)
    tab)

  (define-slot close-tab (widget (index int))
    (declare (connected widget (tab-close-requested int)))
    (declare (method))
    (let ((tab (#_widget widget index)))
      (v:info :parasol "Removing tab ~s" tab)
      (finalize tab))
    NIL)

  (defmethod close-tab ((widget tab-area) (tab tab))
    (finalize tab)
    NIL)

  (define-initializer widget 100
    (#_setMovable widget T)
    (#_setTabsClosable widget T)
    (#_setDocumentMode widget T)

    (add-tab widget welcome))

  (define-finalizer widget 100
    (loop for i from 0 below (#_count widget)
          for tab = (#_widget widget i)
          do (finalize tab))
    (#_clear widget)))
