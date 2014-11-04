#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(named-readtables:in-readtable :qtools)

(with-widget-environment
  (define-widget gizmo-bar (QWidget)
    ())

  (define-layout layout (#_new QHBoxLayout)
    (#_setMinimumSize widget 200 0)))
