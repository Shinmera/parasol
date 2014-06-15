#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass brush-element ()
  ((%name :initarg :name :accessor name)
   (%label :accessor label)
   (%inner :initform (error "INNER required.") :initarg :inner :accessor inner)
   (%on-change :initform (error "ON-CHANGE required.") :initarg :on-change :accessor on-change))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("change(int)" brush-element-change)
          ("change(double)" brush-element-change)
          ("change(const QString)" brush-element-change)
          ("change(bool)" brush-element-change)
          ("change()" brush-element-change)))

(defmethod initialize-instance :after ((widget brush-element) &key)
  (new widget)
  (let ((layout (#_new QHBoxLayout))
        (label (#_new QLabel (name widget))))
    (setf (label widget) label)
    (#_setFixedWidth label 100)
    (#_setSpacing layout 0)
    (#_setContentsMargins layout 0 0 0 0)
    (#_addWidget layout label)
    (#_addWidget layout (inner widget))
    (#_setLayout widget layout)))

(defmethod brush-element-change ((widget brush-element) &optional val)
  (funcall (on-change widget) val))

(defmethod finalize ((widget brush-element))
  (cleanup (widget) label inner))

(defmethod build-brush-element ((type (eql :integer)) name &key range (slot name) (setter #'set-brush-slot))
  (destructuring-bind (from to &optional (step 1)) range
    (let* ((slider (make-instance 'ex-slider-widget :min (round from) :max (round to) :step (round step) :default (slot-value (current-brush *window*) slot)))
           (element (make-instance 'brush-element :inner slider :name (string-downcase name)
                                   :on-change #'(lambda (int) (funcall setter slot int)))))
      (connect slider "valueChanged(int)" element "change(int)")
      element)))

(defmethod build-brush-element ((type (eql :float)) name &key range (slot name) (setter #'set-brush-slot))
  (destructuring-bind (from to &optional (step 0.01)) range
    (let* ((slider (make-instance 'ex-slider-widget :divisor 100.0 :min from :max to :step step :default (slot-value (current-brush *window*) slot)))
           (element (make-instance 'brush-element :inner slider :name (string-downcase name)
                                                  :on-change #'(lambda (double) (funcall setter slot double)))))
      (connect slider "valueChanged(double)" element "change(double)")
      element)))

(defmethod build-brush-element ((type (eql :boolean)) name &key (slot name) (setter #'set-brush-slot))
  (let* ((button (#_new QPushButton))
         (element (make-instance 'brush-element :inner button :name (string-downcase name)
                                                :on-change #'(lambda (toggle) (funcall setter slot toggle)))))
    (when (slot-value (current-brush *window*) slot)
      (#_setDown button T))
    (#_setCheckable button T)
    (connect button "toggled(bool)" element "change(bool)")
    element))

(defmethod build-brush-element ((type (eql :file)) name &key (filters "Any File (*)") (slot name) (setter #'set-brush-slot))
  (let* ((file (make-instance 'file-chooser-widget :file-filters filters :file (slot-value (current-brush *window*) slot)))
         (element (make-instance 'brush-element :inner file :name (string-downcase name)
                                                :on-change #'(lambda (path) (funcall setter slot (uiop:parse-native-namestring path))))))
    (connect file "fileChanged(const QString)" element "change(const QString)")
    element))

(defmethod build-brush-element ((type (eql :string)) name &key (slot name) (setter #'set-brush-slot))
  (let* ((line (#_new QLineEdit))
         (element (make-instance 'brush-element :inner line :name (string-downcase name)
                                                :on-change #'(lambda (nothing) (declare (ignore nothing))
                                                               (funcall setter slot (#_text line))))))
    (connect line "editingFinished()" element "change()")
    element))

(defmethod brush-ui ((brush abstract-brush))
  (loop for field in (class-fields (class-of brush))
        collect (apply #'build-brush-element (getf (cdr field) :type) (car field) (cdr field))))
