#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(define-widget history-list (QTableWidget)
  ())

(define-initializer (history-list setup)
  (#_setSelectionMode history-list (#_QAbstractItemView::SingleSelection)) 
  (#_setSelectionBehavior history-list (#_QAbstractItemView::SelectRows))
  (#_setStretchLastSection (#_horizontalHeader history-list) T)
  (#_hide (#_horizontalHeader history-list))
  (#_hide (#_verticalHeader history-list))
  (#_setColumnCount history-list 1))

(define-widget history-gizmo (QDockWidget)
  ())

(define-subwidget (history-gizmo central) (#_new QWidget history-gizmo)
  (#_setWindowTitle history-gizmo "History")
  (#_setWidget history-gizmo central))

(define-subwidget (history-gizmo list) (make-instance 'history-list))

(define-subwidget (history-gizmo layout) (#_new QGridLayout central)
  (#_setLayout central layout)
  (#_setMargin layout 0)
  (#_setSpacing layout 0)
  (#_setAlignment layout (#_Qt::AlignTop))

  (#_addWidget layout history-gizmo 0 0 1 1))

(define-initializer (history-gizmo setup)
  (#_setSizePolicy history-gizmo (#_QSizePolicy::Maximum) (#_QSizePolicy::Minimum))
  (#_addDockWidget *window* (#_Qt::RightDockWidgetArea) history-gizmo)
  (#_tabifyDockWidget *window* history-gizmo (slot-value *window* 'layer-gizmo)))
