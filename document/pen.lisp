#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)

(defclass pen ()
  ((pointer :initarg :pointer :accessor pointer)
   (device :initarg :device :accessor device)
   (before :initarg :before :accessor before)
   (x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (z :initarg :z :accessor z)
   (x-tilt :initarg :x-tilt :accessor x-titlt)
   (y-tilt :initarg :y-tilt :accessor y-titlt)
   (rotation :initarg :rotation :accessor rotation)
   (pressure :initarg :pressure :accessor pressure)
   (tangential-pressure :initarg :tangential-pressure :accessor tangential-pressure))
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
   :tangential-pressure 0))

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
                                     (tangential-pressure 'tangential-pressure)) &body body)
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
                    (,tangential-pressure tangential-pressure)) ,pen
     ,@body))
