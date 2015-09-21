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

(define-widget document-view (QGLWidget tab positioned)
  ((document :initarg :document :initform NIL :accessor document :finalized T)
   (angle :initarg :angle :initform 0 :accessor angle)
   (zoom :initarg :scale :initform 1.0 :accessor zoom)
   (mirror-x :initarg :mirror-x :initform NIL :accessor mirror-x)
   (mirror-y :initarg :mirror-y :initform NIL :accessor mirror-y)
   (pen :initform NIL :accessor pen)
   (%tablet-input :initform NIL :accessor %tablet-input)
   (pen-pressed :initform NIL :accessor pen-pressed)))

(define-initializer (document-view setup)
  (unless (document document-view)
    (setf (document document-view)
          (make-instance 'document))))

(define-override (document-view paint-event) (event)
  (declare (ignore event))
  (let ((*context* (q+:context document-view)))
    (with-painter (painter document-view)
      (q+:fill-rect painter (q+:rect document-view) (q+:qt.white))
      (with-transformation (painter)
        (translate-to document-view painter)
        ;; origin fix...
        (q+:rotate painter angle)
        (q+:scale painter
                 (if mirror-x (- zoom) zoom)
                 (if mirror-y (- zoom) zoom))
        (draw document painter)))))

(defun translate-x (x document-view)
  (/ (- x (x document-view)) (zoom document-view)))

(defun translate-y (y document-view)
  (/ (- y (y document-view)) (zoom document-view)))

(define-override (document-view tablet-event) (event)
  (setf pen
        (make-instance
         'view-pen
         :pointer (qt:enum-value (q+:pointer-type event))
         :device (qt:enum-value (q+:device event))
         :before pen
         :x (translate-x (q+:x event) document-view)
         :y (translate-y (q+:y event) document-view)
         :z (q+:z event)
         :x-tilt (q+:x-tilt event)
         :y-tilt (q+:y-tilt event)
         :x-view (q+:x event)
         :y-view (q+:y event)
         :rotation (q+:rotation event)
         :pressure (q+:pressure event)
         :tangential-pressure (q+:tangential-pressure event))
        %tablet-input T)
  (q+:ignore event))

(defun maybe-update-pen (document-view event)
  (unless (%tablet-input document-view)
    (setf (pen document-view)
          (make-instance
           'view-pen
           :pointer 1
           :device 0
           :before (pen document-view)
           :x (translate-x (q+:x event) document-view)
           :y (translate-y (q+:y event) document-view)
           :x-view (q+:x event)
           :y-view (q+:y event)
           :pressure *mouse-pressure*))))

(defun process-mouse (document-view event func)
  (maybe-update-pen document-view event)
  (let ((*context* (q+:context document-view)))
    (when (and (tool *window*) (slot-value document-view 'pen-pressed))
      (funcall func (tool *window*) (pen document-view) (document document-view))))
  ;; !Critical
  (q+:repaint document-view)
  (q+:ignore event))

(define-override (document-view mouse-move-event) (event)
  (process-mouse document-view event #'move))

(define-override (document-view mouse-press-event) (event)
  (setf pen-pressed T)
  (process-mouse document-view event #'begin))

(define-override (document-view mouse-release-event) (event)
  (setf pen-pressed NIL
        %tablet-input NIL)
  (process-mouse document-view event #'end))
