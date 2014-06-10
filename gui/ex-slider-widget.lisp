#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass ex-slider-widget ()
  ((%slider :initform (#_new QSlider (#_Qt::Horizontal)) :accessor exs-slider)
   (%spin-box :initform (#_new QSpinBox) :accessor exs-spin-box)
   (%button :initform (#_new QPushButton) :accessor exs-button)
   (%max :initform 100 :initarg :max :accessor exs-max)
   (%min :initform 0 :initarg :min :accessor exs-min)
   (%step :initform 1 :initarg :step :accessor exs-step)
   (%default :initform 0 :initarg :default :accessor exs-default)
   (%on-change :initform #'(lambda (x) (declare (ignore x))) :initarg :on-change :accessor exs-on-change)
   (%on-release :initform #'(lambda (x) (declare (ignore x))) :initarg :on-release :accessor exs-on-release))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("update(int)" exs-update)
          ("release()" exs-release)
          ("reset()" exs-reset)))

(defmethod initialize-instance :after ((widget ex-slider-widget) &key)
  (new widget)
  (#_setTickInterval (exs-slider widget) (exs-step widget))
  (#_setMaximum (exs-slider widget) (exs-max widget))
  (#_setMinimum (exs-slider widget) (exs-min widget))
  (#_setValue (exs-slider widget) (exs-default widget))
  (#_setSingleStep (exs-spin-box widget) (exs-step widget))
  (#_setMaximum (exs-spin-box widget) (exs-max widget))
  (#_setMinimum (exs-spin-box widget) (exs-min widget))
  (#_setValue (exs-spin-box widget) (exs-default widget))
  (#_setText (exs-button widget) (princ-to-string (exs-default widget)))
  (#_setFixedWidth (exs-button widget) 50)
  (connect (exs-slider widget) "valueChanged(int)" widget "update(int)")
  (connect (exs-slider widget) "sliderReleased()" widget "release()")
  (connect (exs-spin-box widget) "valueChanged(int)" widget "update(int)")
  (connect (exs-button widget) "clicked()" widget "reset()")
  (connect (exs-button widget) "clicked()" widget "release()")
  (let ((layout (#_new QHBoxLayout)))
    (#_setSpacing layout 0)
    (#_setContentsMargins layout 0 0 0 0)
    (#_addWidget layout (exs-slider widget) 8)
    (#_addWidget layout (exs-spin-box widget) 1)
    (#_addWidget layout (exs-button widget) 1)
    (#_setLayout widget layout)))

(defmethod exs-update ((widget ex-slider-widget) value)
  (when (or (/= (#_value (exs-slider widget)) value)
            (/= (#_value (exs-spin-box widget)) value))
    (#_setValue (exs-slider widget) value)
    (#_setValue (exs-spin-box widget) value)
    (funcall (exs-on-change widget) value)))

(defmethod exs-release ((widget ex-slider-widget))
  (funcall (exs-on-release widget) (#_value (exs-slider widget))))

(defmethod exs-reset ((widget ex-slider-widget))
  (#_setValue (exs-slider widget) (exs-default widget))
  (#_setValue (exs-spin-box widget) (exs-default widget)))

(defmethod value ((widget ex-slider-widget))
  (#_value (exs-slider widget)))

(defmethod finalize ((widget ex-slider-widget))
  (maybe-delete-qobject (exs-slider widget))
  (maybe-delete-qobject (exs-spin-box widget))
  (maybe-delete-qobject (exs-button widget))
  (setf (exs-slider widget) NIL
        (exs-spin-box widget) NIL
        (exs-button widget) NIL
        (exs-max widget) NIL
        (exs-min widget) NIL
        (exs-step widget) NIL
        (exs-default widget) NIL
        (exs-on-change widget) NIL
        (exs-on-release widget) NIL))
