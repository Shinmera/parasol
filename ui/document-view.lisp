#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(defvar *mouse-pressure* 0.5)
(defvar *context* NIL)

(defclass view-pen (pen)
  ((x-view :initarg :x-view :initform 0 :accessor x-view)
   (y-view :initarg :y-view :initform 0 :accessor y-view)))

(with-widget-environment
  (define-widget document-view (QGLWidget tab positioned)
    ((document :initarg :document :initform NIL :accessor document :finalized T)
     (angle :initarg :angle :initform 0 :accessor angle)
     (zoom :initarg :scale :initform 1.0 :accessor zoom)
     (mirror-x :initarg :mirror-x :initform NIL :accessor mirror-x)
     (mirror-y :initarg :mirror-y :initform NIL :accessor mirror-y)
     (pen :initform NIL :accessor pen)
     (%tablet-input :initform NIL :accessor %tablet-input)
     (pen-pressed :initform NIL :accessor pen-pressed)))

  (define-initializer widget 100
    (unless (document widget)
      (setf (document widget)
            (make-instance 'document))))

  (define-override paint-event (widget event)
    (declare (ignore event))
    (let ((*context* (#_context widget)))
      (with-painter (painter widget)
        (#_fillRect painter (#_rect widget) (#_Qt::white))
        (with-transformation (painter)
          (translate-to widget painter)
          ;; origin fix...
          (#_rotate painter angle)
          (#_scale painter
                   (if mirror-x (- zoom) zoom)
                   (if mirror-y (- zoom) zoom))
          (draw document painter)))))

  (defun translate-x (x widget)
    (/ (- x (x widget)) (zoom widget)))

  (defun translate-y (y widget)
    (/ (- y (y widget)) (zoom widget)))

  (define-override tablet-event (widget event)
    (setf pen
          (make-instance
           'view-pen
           :pointer (qt:enum-value (#_pointerType event))
           :device (qt:enum-value (#_device event))
           :before pen
           :x (translate-x (#_x event) widget)
           :y (translate-y (#_y event) widget)
           :z (#_z event)
           :x-tilt (#_xTilt event)
           :y-tilt (#_yTilt event)
           :x-view (#_x event)
           :y-view (#_y event)
           :rotation (#_rotation event)
           :pressure (#_pressure event)
           :tangential-pressure (#_tangentialPressure event))
          %tablet-input T)
    (#_ignore event))

  (defun maybe-update-pen (widget event)
    (unless (%tablet-input widget)
      (setf (pen widget)
            (make-instance
             'view-pen
             :pointer 1
             :device 0
             :before (pen widget)
             :x (translate-x (#_x event) widget)
             :y (translate-y (#_y event) widget)
             :x-view (#_x event)
             :y-view (#_y event)
             :pressure *mouse-pressure*))))

  (defun process-mouse (widget event func)
    (maybe-update-pen widget event)
    (let ((*context* (#_context widget)))
      (when (and (tool *window*) (slot-value widget 'pen-pressed))
        (funcall func (tool *window*) (pen widget) (document widget))))
    ;; !Critical
    (#_repaint widget)
    (#_ignore event))

  (define-override mouse-move-event (widget event)
    (process-mouse widget event #'move))

  (define-override mouse-press-event (widget event)
    (setf pen-pressed T)
    (process-mouse widget event #'begin))

  (define-override mouse-release-event (widget event)
    (setf pen-pressed NIL
          %tablet-input NIL)
    (process-mouse widget event #'end)))
