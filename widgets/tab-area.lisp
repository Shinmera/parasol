#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(named-readtables:in-readtable :qtools)

(with-widget-environment
  (define-widget welcome-tab (QWidget)
    ())

  (define-subwidget label (#_new QLabel "Welcome to Parasol.")
    (#_setStyleSheet label "background-color:white; color:black;")
    (#_setAlignment label (#_Qt::AlignCenter)))

  (define-layout layout (#_new QHBoxLayout)
    (#_addWidget layout label)))

(with-widget-environment
  (define-widget tab-area (QTabWidget)
    ())

  (define-slot tab-change (widget (index int))
    (declare (connected widget (current-changed int)))
    (let ((tab (#_widget widget index)))
      (v:info :parasol "Switching to tab ~s" tab)))

  (define-slot tab-close (widget (index int))
    (declare (connected widget (tab-close-requested int)))
    (let ((tab (#_widget widget index)))
      (v:info :parasol "Removing tab ~s" tab)
      (#_removeTab widget index)
      (finalize tab)))

  (define-subwidget welcome (make-instance 'welcome-tab))

  (define-initializer widget 100
    (#_setMovable widget T)
    (#_setTabsClosable widget T)
    (#_setDocumentMode widget T)

    (#_addTab widget welcome "Welcome"))

  (define-finalizer widget 100
    (loop for i from 0 below (#_count widget)
          for tab = (#_widget widget i)
          do (finalize tab))
    (#_clear widget)))
