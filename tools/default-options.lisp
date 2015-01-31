#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.tools)
(named-readtables:in-readtable :qtools)

;; A fillable widget
(define-tool-option widget-option (QWidget)
  ())

(define-subwidget (widget-option layout) (#_new QVBoxLayout widget)
  (#_setLayout widget-option layout))

;; Integer spin box
(define-tool-option integer-option (QSpinBox)
  ((min  :initarg :min :initform 0)
   (max  :initarg :max :initform 100)
   (step :initarg :step :initform 1)
   (default :initarg :default :initform 0)))

(define-initializer (integer-option setup)
  (#_setRange integer-option min max)
  (#_setSingleStep integer-option step)
  (#_setValue integer-option default)
  (connect! integer-option (value-changed int) integer-option (change int)))

;; Double spin box
(define-tool-option double-option (QDoubleSpinBox)
  ((min  :initarg :min :initform 0)
   (max  :initarg :max :initform 1.0)
   (step :initarg :step :initform 0.01)
   (default :initarg :default :initform 0.0)))

(define-initializer (double-option setup)
  (#_setRange double-option min max)
  (#_setSingleStep double-option step)
  (#_setValue double-option default)
  (connect! double-option (value-changed double) double-option (change double)))

;; Toggle button
(define-tool-option boolean-option (QPushButton)
  ((default :initarg :default :initform NIL)))

(define-initializer (boolean-option setup)
  (#_setText boolean-option (label boolean-option))
  (#_setDown boolean-option default)
  (connect! boolean-option (toggled bool) boolean-option (change bool)))

;; Simple string input
(define-tool-option string-option (QLineEdit)
  ((default :initarg :default :initform "" :accessor default)))

(define-slot (string-option on-return) ()
  (signal! string-option change ((#_text string-option) string)))

(define-initializer (string-option setup)
  (#_setText string-option default)
  (connect! string-option (return-pressed) string-option (on-return)))

;; Ranged slider
(define-tool-option range-option (QSlider)
  ((min  :initarg :min :initform 0)
   (max  :initarg :max :initform 100)
   (step :initarg :step :initform 1)
   (default :initarg :default :initform 0)))

(define-slot (range-option on-release) ()
  (signal! range-option change ((#_value range-option) int)))

(define-initializer (range-option setup)
  (#_setRange range-option min max)
  (#_setTickInterval range-option step)
  (#_setValue range-option default)
  (connect! range-option (slider-released) range-option (on-release)))

;; Color picker
(define-tool-option color-option (QPushButton)
  ((default :initarg :default :initform (#_new QColor 0 0 0 0) :accessor default)))

(defun %update-background (color-option)
  (let ((pal (#_palette color-option)))
    (#_setColor pal (#_QPalette::Button) (default color-option))
    (#_setPalette color-option pal)
    (#_setAutoFillBackground color-option T)
    (#_update color-option)))

(define-slot (color-option on-click) ()
  (let ((color (#_QColorDialog::getColor default color-option)))
    (when (#_isValid color)
      (setf default color)
      (%update-background color-option)
      (change color-option color))))

(define-initializer (color-option setup)
  (#_setFlat color-option T)
  (%update-background color-option)
  (connect! color-option (clicked) color-option (on-click)))

;; Dropdown list
(define-tool-option list-option (QComboBox)
  ((items :initarg :items :initform ())
   (default :initarg :default :initform NIL)))

(define-initializer (list-option setup)
  (dolist (item items)
    (#_addItem list-option item))
  (when default
    (#_setCurrentIndex list-option (#_findData list-option default)))
  (connect! list-option (current-index-changed string) list-option (change string)))

(defgeneric items (items)
  (:method ((option list-option))
    (slot-value option 'items)))

(defgeneric (setf items) (list items)
  (:method (list (option list-option))
    (setf (slot-value option 'items) list)
    (#_clear option)
    (dolist (item (slot-value option 'items))
      (#_addItem option item))))

(defgeneric add-item (item target)
  (:method (item (option list-option))
    (push item (items option))))

(defgeneric clear-items (target)
  (:method ((option list-option))
    (setf (items option) ())))
