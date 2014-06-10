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
  (let ((document (#_widget widget index)))
    (when (qobject-alive-p document) ;; Singal can occur during finalize.
      (make-active document))))

(defmethod close-tab ((widget documents-widget) index)
  (let ((document (#_widget widget index)))
    (when (destroy document)
      (#_removeTab widget index)
      (maybe-delete-qobject document))))

(defmethod current-document ((widget documents-widget))
  (#_currentWidget widget))

(defmethod finalize ((widget documents-widget))
  (loop for i from 0 below (#_count widget)
        for document = (#_widget widget i)
        do (finalize document)
           (maybe-delete-qobject document))
  (#_clear widget))
