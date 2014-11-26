#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(with-widget-environment
  (define-widget history-list (QTableWidget)
    ())

  (define-initializer list 100
    (#_setSelectionMode list (#_QAbstractItemView::SingleSelection)) 
    (#_setSelectionBehavior list (#_QAbstractItemView::SelectRows))
    (#_setStretchLastSection (#_horizontalHeader list) T)
    (#_hide (#_horizontalHeader list))
    (#_hide (#_verticalHeader list))
    (#_setColumnCount list 1)))

(with-widget-environment
  (define-widget history-gizmo (QDockWidget)
    ())

  (define-subwidget central (#_new QWidget widget)
    (#_setWindowTitle widget "History")
    (#_setWidget widget central))

  (define-subwidget list (make-instance 'history-list))

  (define-subwidget layout (#_new QGridLayout central)
    (#_setLayout central layout)
    (#_setMargin layout 0)
    (#_setSpacing layout 0)
    (#_setAlignment layout (#_Qt::AlignTop))

    (#_addWidget layout list 0 0 1 1))

  (define-initializer widget 100
    (#_setSizePolicy widget (#_QSizePolicy::Maximum) (#_QSizePolicy::Minimum))
    (#_addDockWidget *window* (#_Qt::RightDockWidgetArea) widget)))
