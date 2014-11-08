#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(named-readtables:in-readtable :qtools)

(with-widget-environment
  (define-widget document-view (QWidget tab)
    ((document :initarg :document :initform NIL :accessor document :finalized T)
     (x :initarg :x :initform 0 :accessor x)
     (y :initarg :y :initform 0 :accessor y)
     (angle :initarg :angle :initform 0 :accessor angle)
     (zoom :initarg :scale :initform 1.0 :accessor zoom)))

  (define-override paint-event (widget event)
    (declare (ignore event))
    (with-finalizing ((painter (#_new QPainter widget)))
      (#_translate painter x y)
      (#_rotate painter angle)
      (#_scale painter zoom zoom)
      (draw document painter)))

  (define-override tablet-event (widget event)
    (#_ignore event))

  (define-override mouse-move-event (widget event)
    (#_ignore event))

  (define-override mouse-press-event (widget event)
    (#_ignore event))

  (define-override mouse-release-event (widget event)
    (#_ignore event)))
