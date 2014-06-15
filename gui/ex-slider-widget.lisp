#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass ex-slider-widget ()
  ((%slider :accessor exs-slider)
   (%spin-box :accessor exs-spin-box)
   (%button  :accessor exs-button)
   (%max :initform 100 :initarg :max :accessor exs-max)
   (%min :initform 0 :initarg :min :accessor exs-min)
   (%step :initform 1 :initarg :step :accessor exs-step)
   (%default :initform 0 :initarg :default :accessor exs-default)
   (%divisor :initform 1 :initarg :divisor :accessor exs-divisor))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("update(int)" exs-update)
          ("update(double)" exs-update)
          ("release()" exs-release)
          ("reset()" exs-reset))
  (:signals ("valueChanged(int)")
            ("onRelease(int)")
            ("valueChanged(double)")
            ("onRelease(double)")))

(defmethod initialize-instance :after ((widget ex-slider-widget) &key)
  (new widget)
  (let* ((divisor (exs-divisor widget))
         (slider (#_new QSlider (#_Qt::Horizontal)))
         (spin-box (if (= 1 divisor) (#_new QSpinBox) (#_new QDoubleSpinBox)))
         (button (#_new QPushButton)))
    (setf (exs-slider widget) slider
          (exs-spin-box widget) spin-box
          (exs-button widget) button)
    (#_setTickInterval slider (round (* divisor (exs-step widget))))
    (#_setMaximum slider (round (* divisor (exs-max widget))))
    (#_setMinimum slider (round (* divisor (exs-min widget))))
    (#_setValue slider (round (* divisor (exs-default widget))))
    (#_setSingleStep spin-box (exs-step widget))
    (#_setMaximum spin-box (exs-max widget))
    (#_setMinimum spin-box (exs-min widget))
    (#_setValue spin-box (exs-default widget))
    (#_setText button (princ-to-string (exs-default widget)))
    (#_setFixedWidth button 50)
    (connect slider "valueChanged(int)" widget "update(int)")
    (connect slider "sliderReleased()" widget "release()")
    (if (= 1 divisor)
        (connect spin-box "valueChanged(int)" widget "update(int)")
        (connect spin-box "valueChanged(double)" widget "update(double)"))
    (connect button "clicked()" widget "reset()")
    (connect button "clicked()" widget "release()")
    (let ((layout (#_new QHBoxLayout)))
      (#_setSpacing layout 0)
      (#_setContentsMargins layout 0 0 0 0)
      (#_addWidget layout slider 8)
      (#_addWidget layout spin-box 1)
      (#_addWidget layout button 1)
      (#_setLayout widget layout))))

(defmethod exs-update ((widget ex-slider-widget) value)
  (when (or (/= (#_value (exs-slider widget)) value)
            (/= (#_value (exs-spin-box widget)) value))
    (if (= 1 (exs-divisor widget))
        (progn
          (when (and (= (#_value (exs-spin-box widget)) value)
                     (/= (#_value (exs-slider widget)) value))
            (emit-signal widget "onRelease(int)" value))
          (#_setValue (exs-slider widget) value)
          (#_setValue (exs-spin-box widget) value)
          (emit-signal widget "valueChanged(int)" value))
        (progn
          (when (and (= (#_value (exs-spin-box widget)) value)
                     (/= (#_value (exs-slider widget)) value))
            (emit-signal widget "onRelease(double)" value))
          (when (= (#_value (exs-slider widget)) value)
            (setf value (/ value (exs-divisor widget))))
          (#_setValue (exs-slider widget) (round (* value (exs-divisor widget))))
          (#_setValue (exs-spin-box widget) value)
          (emit-signal widget "valueChanged(double)" value)))))

(defmethod exs-release ((widget ex-slider-widget))
  (emit-signal widget "onRelease(int)" (#_value (exs-slider widget))))

(defmethod exs-reset ((widget ex-slider-widget))
  (#_setValue (exs-slider widget) (exs-default widget))
  (#_setValue (exs-spin-box widget) (exs-default widget)))

(defmethod value ((widget ex-slider-widget))
  (#_value (exs-spin-box widget)))

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
        (exs-default widget) NIL))
