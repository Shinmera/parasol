#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)

(defun pointer-name (int)
  (ecase int
    (0 :unknown)
    (1 :pen)
    (2 :cursor)
    (3 :eraser)))

(defun device-name (int)
  (ecase int
    (0 :unknown)
    (1 :puck)
    (2 :stylus)
    (3 :airbrush)
    (4 :4d-mouse)
    (6 :rotation-stylus)))

(defclass pen ()
  ((pointer :initarg :pointer :accessor pointer)
   (device :initarg :device :accessor device)
   (before :initarg :before :accessor before)
   (x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (z :initarg :z :accessor z)
   (x-tilt :initarg :x-tilt :accessor x-tilt)
   (y-tilt :initarg :y-tilt :accessor y-tilt)
   (rotation :initarg :rotation :accessor rotation)
   (pressure :initarg :pressure :accessor pressure)
   (tangential-pressure :initarg :tangential-pressure :accessor tangential-pressure)
   (real-time :initarg :real-time :accessor real-time))
  (:default-initargs
   :pointer 0
   :device 0
   :before NIL
   :x 0
   :y 0
   :z 0
   :x-tilt 0
   :y-tilt 0
   :rotation 0
   :pressure 0
   :tangential-pressure 0
   :real-time (get-internal-real-time)))

(defmethod print-object ((pen pen) stream)
  (print-unreadable-object (pen stream :type T)
    (with-pen-values (pen)
      (format stream "~a:~a ~a/~a (~a)"
              (device-name device) (pointer-name pointer)
              x y real-time))))

(defmethod copy ((pen pen))
  (with-pen-values (pen)
    (make-instance
     'pen
     :pointer pointer
     :device device
     :before before
     :x x
     :y y
     :z z
     :x-tilt x-tilt
     :y-tilt y-tilt
     :rotation rotation
     :pressure pressure
     :tangential-pressure tangential-pressure
     :real-time real-time)))

(defun diff (slot pen)
  (if (before pen)
      (- (slot-value pen slot)
         (slot-value (before pen) slot))
      0))

(defmacro with-pen-values ((pen &key (pointer 'pointer)
                                     (device 'device)
                                     (before 'before)
                                     (x 'x)
                                     (y 'y)
                                     (z 'z)
                                     (x-tilt 'x-tilt)
                                     (y-tilt 'y-tilt)
                                     (rotation 'rotation)
                                     (pressure 'pressure)
                                     (tangential-pressure 'tangential-pressure)
                                     (real-time 'real-time)) &body body)
  `(with-accessors ((,pointer pointer)
                    (,device device)
                    (,before before)
                    (,x x)
                    (,y y)
                    (,z z)
                    (,x-tilt x-tilt)
                    (,y-tilt y-tilt)
                    (,rotation rotation)
                    (,pressure pressure)
                    (,tangential-pressure tangential-pressure)
                    (,real-time real-time)) ,pen
     (declare (ignorable ,pointer
                         ,device
                         ,before
                         ,x
                         ,y
                         ,z
                         ,x-tilt
                         ,y-tilt
                         ,rotation
                         ,pressure
                         ,tangential-pressure
                         ,real-time))
     ,@body))
