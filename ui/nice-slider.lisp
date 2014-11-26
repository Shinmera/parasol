#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(with-widget-environment
  (define-widget double-slider (QSlider)
    ((max :initform 100.0 :initarg :max)
     (min :initform 100.0 :initarg :min)
     (step :initform 1.0 :initarg :step)
     (div)))

  (define-initializer slider 100
    (print
     (setf div (let ((str (string-trim "0" (format NIL "~f" step))))
                 (expt 10 (- (length str) (position #\. str) 1)))))
    (#_setMaximum slider (round (* div max)))
    (#_setMinimum slider (round (* div min)))
    (#_setTickInterval slider (round (* div step)))
    (#_setOrientation slider (#_Qt::Horizontal)))

  (define-signal value-changed (double))

  (define-slot update (slider (value int))
    (declare (connected slider (value-changed int)))
    (signal! slider value-changed ((/ value div) double)))

  (defmethod value ((slider double-slider))
    (/ (#_value slider) (slot-value slider 'div)))

  (defmethod (setf value) (value (slider double-slider))
    (#_setValue slider (round (* value (slot-value slider 'div))))))

(with-widget-environment
  (define-widget nice-slider (QWidget)
    ((max :initform 100.0 :initarg :max)
     (min :initform 0.0 :initarg :min)
     (step :initform 1.0 :initarg :step)
     (default :initform 0.0 :initarg :default)))
  
  (define-signal value-changed (double))
  
  (define-subwidget slider (make-instance 'double-slider :max max :min min :step step)
    (setf (value slider) default))
  
  (define-subwidget spin-box (#_new QDoubleSpinBox)
    (#_setSingleStep spin-box step)
    (#_setMaximum spin-box max)
    (#_setMinimum spin-box min)
    (#_setValue spin-box default)
    (#_setFixedWidth spin-box 70))
  
  (define-subwidget button (#_new QPushButton)
    (#_setText button (princ-to-string default))
    (#_setFixedWidth button 50))

  (define-layout layout (#_new QHBoxLayout)
    (#_setSpacing layout 0)
    (#_setContentsMargins layout 0 0 0 0)
    (#_addWidget layout slider 8)
    (#_addWidget layout spin-box 1)
    (#_addWidget layout button 1))

  (define-slot update (widget (value double))
    (declare (connected slider (value-changed double)))
    (declare (connected spin-box (value-changed double)))
    (when (or (/= (value slider) value)
              (/= (value spin-box) value))
      (setf (value slider) value)
      (setf (value spin-box) value)
      (signal! widget value-changed (value double))))

  (define-slot reset (widget)
    (declare (connected button (clicked)))
    (setf (value slider) default)
    (setf (value spin-box) default))

  (defmethod value ((widget nice-slider))
    (#_value (slot-value widget 'spin-box)))

  (defmethod (setf value) (value (widget nice-slider))
    (with-slots-bound (widget nice-slider)
      (setf (value spin-box) value)
      (setf (value slider) value))))
