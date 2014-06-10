#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass repl-widget ()
  ((%input :initform (#_new QLineEdit) :accessor input))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("replEval()" repl-eval)))

(defmethod initialize-instance :after ((widget repl-widget) &key)
  (new widget)
  (let ((layout (#_new QHBoxLayout))
        (label (#_new QLabel ">")))
    (#_setFont widget (#_new QFont "Consolas" 10))
    (#_setFixedHeight widget 30)
    (#_setFrame (input widget) NIL)
    (#_setFixedWidth label 10)
    (#_setSpacing layout 0)
    (#_setContentsMargins layout 10 0 0 0)
    (#_addWidget layout label)
    (#_addWidget layout (input widget))
    (#_setLayout widget layout)
    (connect (input widget) "returnPressed()" widget "replEval()")))

(defmethod repl-eval ((widget repl-widget))
  
  (handler-case
      (let ((read (read-from-string (#_text (input widget)))))
        (handler-case
            (#_setPlaceholderText
             (input widget) (princ-to-string (eval read)))
          (error (err)
            (#_setPlaceholderText
             (input widget) (format NIL "; EVAL error: ~a" err)))))
    (error (err)
      (#_setPlaceholderText
       (input widget) (format NIL "; READ error: ~a" err))))
  (#_clear (input widget))
  (#_clearFocus (input widget)))
