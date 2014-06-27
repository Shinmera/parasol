#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.parasol)

(defclass linear (curve)
  ((%interpolated :initform (make-array 5 :adjustable T :fill-pointer 0) :accessor interpolated)
   (%remainder :initform 0 :accessor remainder)))

(defmethod initialize-instance :after ((curve linear) &key)
  (loop repeat (array-dimension (data curve) 0)
        do (vector-push-extend (make-array 0 :element-type 'float :initial-element 0.0 :adjustable T :fill-pointer T) (data curve))
           (vector-push-extend (make-array 0 :element-type 'float :initial-element 0.0 :adjustable T :fill-pointer T) (interpolated curve))))

(defmethod (setf point-distance) (point-distance (curve linear))
  (setf (slot-value curve '%point-distance)
        point-distance)
  (loop for int across (interpolated curve)
        do (setf (fill-pointer int) 0))
  (recalculate-linear-interpolations curve))

(defun calculate-linear-distances (curve)
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

;; This function is gross.
(defun recalculate-linear-interpolations (curve &key (from 0))
  (let* ((dist (distances curve))
         (pointd (point-distance curve))
         (len (length (aref (data curve) 0))))
    (when (< 1 len)
      (loop with i = from
            with seglen = (aref dist i)
            for cdist = (remainder curve)
              then (+ cdist pointd)
            do (loop while (>= cdist seglen)
                     do (incf i)
                        (setf cdist (- cdist seglen))
                        (if (< i (1- len))
                            (setf seglen (aref dist i))
                            (return)))
            while (< i (1- len))
            do (loop for int across (interpolated curve)
                     for dat across (data curve)
                     do (vector-push-extend
                         (+ (aref dat i)
                            (* (/ cdist seglen)
                               (- (aref dat (1+ i))
                                  (aref dat i))))
                         int))
            finally (setf (remainder curve) cdist)))
    (setf (point-amount curve)
          (fill-pointer (aref (interpolated curve) 0)))))

(defmethod record-point ((curve linear) x y x-tilt y-tilt pressure)
  (loop for data in (list x y x-tilt y-tilt pressure)
        for i from 0
        do (vector-push-extend (float data) (elt (data curve) i)))
  (calculate-linear-distances curve)
  (recalculate-linear-interpolations curve :from (- (length (aref (data curve) 0)) 2)))

(defmethod make-curve ((type (eql 'linear)))
  (make-instance 'linear))

(defmethod point-data ((curve linear) pos)
  (list (aref (aref (interpolated curve) 0) pos)
        (aref (aref (interpolated curve) 1) pos)
        (aref (aref (interpolated curve) 2) pos)
        (aref (aref (interpolated curve) 3) pos)
        (aref (aref (interpolated curve) 4) pos)))

(defmethod map-points ((curve linear) function &key from to)
  (unless from (setf from 0))
  (unless to (setf to (point-amount curve)))
  (let ((xs (aref (interpolated curve) 0))
        (ys (aref (interpolated curve) 1))
        (xts (aref (interpolated curve) 2))
        (yts (aref (interpolated curve) 3))
        (ps (aref (interpolated curve) 4)))
    (loop for i from from below to
          do (funcall function (aref xs i) (aref ys i) (aref xts i) (aref yts i) (aref ps i)))))
