#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.parasol)

(defclass base (curve)
  ())

(defmethod (setf point-distance) (point-distance (curve base))
  (setf (slot-value curve '%point-distance)
        point-distance)
  (loop for int across (interpolated curve)
        do (setf (fill-pointer int) 0))
  (recalculate-linear-interpolations curve))

(defun calculate-base-spline-distances (curve)
  ;; Cheat with linear interpolation
  (let* ((data (data curve))
         (dist (distances curve))
         (len (length (aref data 0))))
    (when (< 1 len)
      (flet ((dist (i j)
               (sqrt (+ (expt (- (aref (aref data 0) i)
                                 (aref (aref data 0) j)) 2)
                        (expt (- (aref (aref data 1) i)
                                 (aref (aref data 1) j)) 2)))))
        (loop for i from (length dist) below (1- len)
              do (vector-push-extend (dist i (1+ i)) dist))))))

(defmethod make-curve ((type (eql 'base)))
  (make-instance 'base))

(defmethod record-point ((curve base) x y x-tilt y-tilt pressure)
  (loop for data in (list x y x-tilt y-tilt pressure)
        for i from 0
        do (vector-push-extend (float data) (elt (data curve) i)))
  (calculate-base-spline-distances curve)
  ;;(recalculate-linear-interpolations curve :from (- (length (aref (data curve) 0)) 3))
  )

(defmethod point-data ((spline base) pos)
  (list (aref (aref (interpolated spline) 0) pos)
        (aref (aref (interpolated spline) 1) pos)
        (aref (aref (interpolated spline) 2) pos)
        (aref (aref (interpolated spline) 3) pos)
        (aref (aref (interpolated spline) 4) pos)))

(defmethod map-points ((spline base) function &key from to)
  (unless from (setf from 0))
  (unless to (setf to (point-amount spline)))
  (let ((xs (aref (interpolated spline) 0))
        (ys (aref (interpolated spline) 1))
        (xts (aref (interpolated spline) 2))
        (yts (aref (interpolated spline) 3))
        (ps (aref (interpolated spline) 4)))
    (loop for i from from below to
          do (funcall function (aref xs i) (aref ys i) (aref xts i) (aref yts i) (aref ps i)))))
