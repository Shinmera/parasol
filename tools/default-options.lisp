#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(named-readtables:in-readtable :qtools)

;; A fillable widget
(with-widget-environment
  (define-tool-option widget-option (QWidget)
    ()))

;; Integer spin box
(with-widget-environment
  (define-tool-option integer-option (QSpinBox)
    ((min  :initarg :min :initform 0)
     (max  :initarg :max :initform 100)
     (step :initarg :step :initform 1)
     (default :initarg :default :initform 0)))

  (define-initializer widget 100
    (#_setRange widget min max)
    (#_setSingleStep widget step)
    (#_setValue widget default)
    (connect! widget (value-changed int) widget (change int))))

;; Double spin box
(with-widget-environment
  (define-tool-option double-option (QDoubleSpinBox)
    ((min  :initarg :min :initform 0)
     (max  :initarg :max :initform 1.0)
     (step :initarg :step :initform 0.01)
     (default :initarg :default :initform 0.0)))

  (define-initializer widget 100
    (#_setRange widget min max)
    (#_setSingleStep widget step)
    (#_setValue widget default)
    (connect! widget (value-changed double) widget (change double))))

;; Toggle button
(with-widget-environment
  (define-tool-option boolean-option (QPushButton)
    ((default :initarg :default :initform NIL)))

  (define-initializer widget 100
    (#_setText widget (label widget))
    (#_setDown widget default)
    (connect! widget (toggled bool) widget (change bool))))

;; Simple string input
(with-widget-environment
  (define-tool-option string-option (QLineEdit)
    ((default :initarg :default :initform "" :accessor default)))

  (define-slot on-return (widget)
    (signal! widget change ((#_text widget) string)))

  (define-initializer widget 100
    (#_setText widget default)
    (connect! widget (return-pressed) widget (on-return))))

;; Ranged slider
(with-widget-environment
  (define-tool-option range-option (QSlider)
    ((min  :initarg :min :initform 0)
     (max  :initarg :max :initform 100)
     (step :initarg :step :initform 1)
     (default :initarg :default :initform 0)))

  (define-slot on-release (widget)
    (signal! widget change ((#_value widget) int)))

  (define-initializer widget 100
    (#_setRange widget min max)
    (#_setTickInterval widget step)
    (#_setValue widget default)
    (connect! widget (slider-released) widget (on-release))))

;; Color picker
(with-widget-environment
  (define-tool-option color-option (QPushButton)
    ((default :initarg :default :initform (#_new QColor 0 0 0 0) :accessor default)))

  (defun %update-background (widget)
    (let ((pal (#_palette widget)))
      (#_setColor pal (#_QPalette::Button) (default widget))
      (#_setPalette widget pal)
      (#_setAutoFillBackground widget T)
      (#_update widget)))

  (define-slot on-click (widget)
    (let ((color (#_QColorDialog::getColor default widget)))
      (when (#_isValid color)
        (setf default color)
        (%update-background widget))))
  
  (define-initializer widget 100
    (%update-background widget)
    (connect! widget (clicked) widget (on-click))))

;; Dropdown list
(with-widget-environment
  (define-tool-option list-option (QComboBox)
    ((items :initarg :items :initform ())
     (default :initarg :default :initform NIL)))

  (define-initializer widget 100
    (dolist (item items)
      (#_addItem widget item))
    (when default
      (#_setCurrentIndex widget (#_findData widget default)))
    (#_setTooltip widget (description widget))
    (connect! widget (current-index-changed string) widget (change string)))

  (defmethod add-item (item (option list-option))
    (push item (items option))
    (#_addItem option item)))
