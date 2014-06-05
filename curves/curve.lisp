#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.parasol)

(defclass curve ()
  ()
  (:documentation ""))

(defgeneric record-point (curve x y x-tilt y-tilt pressure)
  (:documentation "Records a new point."))

(defgeneric make-curve (curve-type)
  (:documentation "Creates a new, empty curve of type CURVE-TYPE."))

(defgeneric point-count (curve)
  (:documentation "Returns the number of points the curve wants to draw.
This is not equal to the points recorded in the curve, but rather to the
number of points the curve uses when interpolated."))

(defgeneric point-data (curve pos)
  (:documentation "Returns a list of point-data at position POS.
POS must be of the following size: 0<=POS<POINT-COUNT
The returned list looks like this: (X Y X-TILT Y-TILT PRESSURE)"))

(defgeneric map-points (curve function &key from to)
  (:documentation "Maps the interpolated points of CURVE to FUNCTION.
The function must accept five arguments: X Y X-TILT Y-TILT PRESSURE
FROM and TO must be of the following size: 0<=FROM<TO<=POINT-COUNT
FROM defaults to 0 and TO to POINT-COUNT"))
