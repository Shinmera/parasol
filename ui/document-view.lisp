#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(defvar *mouse-pressure* 0.5)

(with-widget-environment
  (define-widget document-view (QWidget tab positioned)
    ((document :initarg :document :initform NIL :accessor document :finalized T)
     (angle :initarg :angle :initform 0 :accessor angle)
     (zoom :initarg :scale :initform 1.0 :accessor zoom)
     (mirror-x :initarg :mirror-x :initform NIL :accessor mirror-x)
     (mirror-y :initarg :mirror-y :initform NIL :accessor mirror-y)
     (pen :initform NIL :accessor pen)
     (%tablet-input :initform NIL)
     (pen-pressed :initform NIL :accessor pen-pressed)))

  (define-initializer widget 100
    (unless (document widget)
      (setf (document widget)
            (make-instance 'document))))

  (define-override paint-event (widget event)
    (declare (ignore event))
    (with-finalizing ((painter (#_new QPainter widget)))
      (#_setRenderHint painter (#_QPainter::Antialiasing))
      (#_setRenderHint painter (#_QPainter::SmoothPixmapTransform))
      (#_setRenderHint painter (#_QPainter::HighQualityAntialiasing))
      (#_fillRect painter (#_rect widget) (#_Qt::white))
      (with-transformation (painter)
        (translate-away widget painter)
        ;; origin fix...
        (#_rotate painter angle)
        (#_scale painter
                 (if mirror-x (- zoom) zoom)
                 (if mirror-y (- zoom) zoom))
        (draw document painter))))

  (define-override tablet-event (widget event)
    (setf pen
          (make-instance
           'pen
           :pointer (qt:enum-value (#_pointerType event))
           :device (qt:enum-value (#_device event))
           :before pen
           :x (+ (#_x event) (x widget))
           :y (+ (#_y event) (y widget))
           :z (#_z event)
           :x-tilt (#_xTilt event)
           :y-tilt (#_yTilt event)
           :rotation (#_rotation event)
           :pressure (#_pressure event)
           :tangential-pressure (#_tangentialPressure event))
          %tablet-input T)
    (#_ignore event))

  (defun maybe-update-pen (widget event)
    (unless (slot-value widget '%tablet-input)
      (setf (pen widget)
            (make-instance
             'pen
             :pointer 1
             :device 0
             :before (pen widget)
             :x (+ (#_x event) (x widget))
             :y (+ (#_y event) (y widget))
             :pressure *mouse-pressure*))))

  (defun process-mouse (widget event func)
    (maybe-update-pen widget event)
    (when (and (tool *window*) (slot-value widget 'pen-pressed))
      (funcall func (tool *window*) (pen widget) (document widget)))
    ;; !Critical
    (#_repaint widget)
    (#_ignore event))

  (define-override mouse-move-event (widget event)
    (process-mouse widget event #'move))

  (define-override mouse-press-event (widget event)
    (setf pen-pressed T)
    (process-mouse widget event #'begin))

  (define-override mouse-release-event (widget event)
    (setf pen-pressed NIL)
    (process-mouse widget event #'end)))
