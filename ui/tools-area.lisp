#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(with-widget-environment
  (define-widget tools-area (QWidget)
    ())

  (define-subwidget layout (#_new QHBoxLayout widget)
    (#_setSizePolicy widget (#_QSizePolicy::Maximum) (#_QSizePolicy::Maximum))
    (#_setLayout widget layout)
    (#_setMargin layout 0)
    (#_setSpacing layout 0)
    (#_setAlignment layout (#_Qt::AlignLeft))
    (dolist (tool (find-tools))
      (#_addWidget layout (make-instance tool) 0))))
