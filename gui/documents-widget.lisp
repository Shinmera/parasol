#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass documents-widget ()
  ()
  (:metaclass qt-class)
  (:qt-superclass "QTabWidget")
  (:slots ("tabChange(int)" change-tab)
          ("tabClose(int)" close-tab)))

(defmethod initialize-instance :after ((widget documents-widget) &key)
  (new widget)
  (#_setMovable widget T)
  (#_setTabsClosable widget T)
  (#_setDocumentMode widget T)
  (connect widget "currentChanged(int)" widget "tabChange(int)")
  (connect widget "tabCloseRequested(int)" widget "tabClose(int)"))

(defmethod change-tab ((widget documents-widget) index)
  (make-active (#_widget widget index)))

(defmethod close-tab ((widget documents-widget) index)
  (let ((document (#_widget widget index)))
    (when (close-document document)
      (#_removeTab widget index)
      (optimized-delete document))))
