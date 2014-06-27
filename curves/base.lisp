#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.parasol)

(defclass base (curve)
  ((%interpolated :initform (make-array 5 :adjustable T :fill-pointer 0) :accessor interpolated)
   (%remainder :initform 0 :accessor remainder)))

(defmethod initialize-instance :after ((curve base) &key)
  (loop repeat (array-dimension (data curve) 0)
        do (vector-push-extend (make-array 0 :element-type 'float :initial-element 0.0 :adjustable T :fill-pointer T) (data curve))
           (vector-push-extend (make-array 0 :element-type 'float :initial-element 0.0 :adjustable T :fill-pointer T) (interpolated curve))))

(defmethod (setf point-distance) (point-distance (curve base))
  (setf (slot-value curve '%point-distance)
        point-distance)
  (loop for int across (interpolated curve)
        do (setf (fill-pointer int) 0))
  (recalculate-base-spline-interpolations curve))

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

(defun recalculate-base-spline-interpolations (curve &key (from 0))
  (let* ((dist (distances curve))
         (pointd (point-distance curve))
         (len (length (aref (data curve) 0))))
    (when (< 3 len)
      (loop with i = from
            with seglen = (aref dist 0)
            for cdist = (remainder curve)
              then (+ cdist pointd)
            do (loop while (>= cdist seglen)
                     do (incf i)
                        (setf cdist (- cdist seglen))
                        (if (< i (1- len))
                            (setf seglen (elt dist i))
                            (return)))
            while (< (+ i 3) len)
            do (loop for int across (interpolated curve)
                     for dat across (data curve)
                     do (let* ((x1 (aref dat i))
                               (x2 (aref dat (+ i 1)))
                               (x3 (aref dat (+ i 2)))
                               (x4 (aref dat (+ i 3)))
                               (a0 (/ (+ (- x1) (* 3 x2) (- (* 3 x3)) x4) 6))
                               (a1 (/ (+ (* 3 x1) (- (* 6 x2)) (* 3 x3)) 6))
                               (a2 (/ (+ (- (* 3 x1)) (* 3 x3)) 6))
                               (a3 (/ (+ x1 (* 4 x2) x3) 6))
                               (s (/ cdist seglen)))
                          (vector-push-extend
                           (+ a3 (* s (+ a2 (* s (+ a1 (* s a0))))))
                           int)))
            finally (setf (remainder curve) cdist)))
    (setf (point-amount curve)
          (fill-pointer (aref (interpolated curve) 0)))))

(defmethod make-curve ((type (eql 'base)))
  (make-instance 'base))

(defmethod record-point ((curve base) x y x-tilt y-tilt pressure)
  (loop for data in (list x y x-tilt y-tilt pressure)
        for i from 0
        do (vector-push-extend (float data) (elt (data curve) i)))
  (calculate-base-spline-distances curve)
  (recalculate-base-spline-interpolations curve :from (- (length (aref (data curve) 0)) 4)))

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