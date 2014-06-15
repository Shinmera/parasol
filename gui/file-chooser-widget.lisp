#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass file-chooser-widget ()
  ((%file :initarg :file :initform (user-homedir-pathname) :accessor file)
   (%filters :initarg :file-filters :initform "Any File (*)" :accessor filters)
   (%chooser-button :initform (#_new QPushButton "Select..") :accessor chooser-button)
   (%chooser-label :initform (#_new QLineEdit) :accessor chooser-label))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("chooseFile()" file-chooser-choose))
  (:signals ("fileChanged(const QString)")))

(defmethod initialize-instance :after ((widget file-chooser-widget) &key)
  (new widget)
  (let ((layout (#_new QHBoxLayout)))
    (#_setSpacing layout 0)
    (#_setContentsMargins layout 0 0 0 0)
    (#_setMaximumWidth (chooser-button widget) 200)
    (#_setReadOnly (chooser-label widget) T)
    (#_addWidget layout (chooser-label widget))
    (#_addWidget layout (chooser-button widget))
    (#_setLayout widget layout)
    (connect (chooser-button widget) "clicked()" widget "chooseFile()")))

(defmethod file-chooser-choose ((widget file-chooser-widget))
  (let ((path (#_QFileDialog::getOpenFileName widget "Choose File" (uiop:native-namestring (file widget)) (filters widget))))
    (when (< 0 (length path))
      (setf (file widget) (uiop:parse-native-namestring path))
      (#_setText (chooser-label widget) path)
      (#_setCursorPosition (chooser-label widget) 0)
      (qt:emit-signal widget "fileChanged(const QString)" (uiop:native-namestring (file widget))))))


