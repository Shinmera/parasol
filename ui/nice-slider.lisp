#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(define-widget double-slider (QSlider)
  ((max :initform 100.0 :initarg :max)
   (min :initform 100.0 :initarg :min)
   (step :initform 1.0 :initarg :step)
   (div)))

(define-initializer (double-slider setup)
  (setf div (let ((str (string-trim "0" (format NIL "~f" step))))
              (expt 10 (- (length str) (position #\. str) 1))))
  (#_setMaximum double-slider (round (* div max)))
  (#_setMinimum double-slider (round (* div min)))
  (#_setTickInterval double-slider (round (* div step)))
  (#_setOrientation double-slider (#_Qt::Horizontal)))

(define-signal (double-slider value-changed) (double))

(define-slot (double-slider update) ((value int))
  (declare (connected double-slider (value-changed int)))
  (signal! double-slider (value-changed double) (/ value div)))

(defmethod value ((double-slider double-slider))
  (/ (#_value double-slider) (slot-value double-slider 'div)))

(defmethod (setf value) (value (double-slider double-slider))
  (#_setValue double-slider (round (* value (slot-value double-slider 'div)))))

(define-widget nice-slider (QWidget)
  ((max :initform 100.0 :initarg :max)
   (min :initform 0.0 :initarg :min)
   (step :initform 1.0 :initarg :step)
   (default :initform 0.0 :initarg :default)))

(define-signal (nice-slider value-changed) (double))

(define-subwidget (nice-slider slider) (make-instance 'double-slider :max max :min min :step step)
  (setf (value slider) default))

(define-subwidget (nice-slider spin-box) (#_new QDoubleSpinBox)
  (#_setSingleStep spin-box step)
  (#_setMaximum spin-box max)
  (#_setMinimum spin-box min)
  (#_setValue spin-box default)
  (#_setFixedWidth spin-box 70))

(define-subwidget (nice-slider button) (#_new QPushButton)
  (#_setText button (princ-to-string default))
  (#_setFixedWidth button 50))

(define-subwidget (nice-slider layout) (#_new QHBoxLayout)
  (#_setSpacing layout 0)
  (#_setContentsMargins layout 0 0 0 0)
  (#_addWidget layout slider 8)
  (#_addWidget layout spin-box 1)
  (#_addWidget layout button 1)
  (#_setLayout nice-slider layout))

(define-slot (nice-slider update) ((value double))
  (declare (connected slider (value-changed double)))
  (declare (connected spin-box (value-changed double)))
  (when (or (/= (value slider) value)
            (/= (value spin-box) value))
    (setf (value slider) value)
    (setf (value spin-box) value)
    (signal! nice-slider (value-changed double) value)))

(define-slot (nice-slider reset) ()
  (declare (connected button (clicked)))
  (setf (value slider) default)
  (setf (value spin-box) default))

(defmethod value ((nice-slider nice-slider))
  (#_value (slot-value nice-slider 'spin-box)))

(defmethod (setf value) (value (nice-slider nice-slider))
  (with-slots-bound (nice-slider nice-slider)
    (setf (value spin-box) value)
    (setf (value slider) value)))
