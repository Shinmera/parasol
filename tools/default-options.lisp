#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(named-readtables:in-readtable :qtools)

(with-widget-environment
  (define-tool-option integer-option (QSpinBox)
    ()))

(with-widget-environment
  (define-tool-option double-option (QDoubleSpinBox)
    ()))

(with-widget-environment
  (define-tool-option boolean-option (QPushButton)
    ()))

(with-widget-environment
  (define-tool-option string-option (QLineEdit)
    ()))

(with-widget-environment
  (define-tool-option range-option (QSlider)
    ()))

(with-widget-environment
  (define-tool-option list-option (QComboBox)
    ()))

(with-widget-environment
  (define-tool-option color-option (QPushButton)
    ()))
