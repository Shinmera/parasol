#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.parasol)

(defvar *cubic-spline-adjust-buffer* 500)
(defvar *cubic-spline-from-hack* 0)

(defclass cubic (curve)
  ((%spline-data :initform (make-array 5 :adjustable T :fill-pointer 0) :accessor spline-data)
   (%interpolated :initform (make-array 5 :adjustable T :fill-pointer 0) :accessor interpolated)))

(defun ensure-length (vector required-length)
  (cond ((< (array-dimension vector 0) required-length)
         (adjust-array vector (+ required-length *cubic-spline-adjust-buffer*)
                       :fill-pointer required-length))
        ((< (fill-pointer vector) required-length)
         (setf (fill-pointer vector) required-length)))
  vector)

(defmethod print-object ((spline cubic) stream)
  (print-unreadable-object (spline stream :type T :identity T)
    (format stream "(~d/~d)" (length (data spline)) (length (aref (data spline) 0))))
  spline)

(defmethod (setf point-distance) (point-distance (spline cubic))
  (setf (slot-value spline '%point-distance)
        point-distance)
  (when (/= (length (distances spline)) 0)
    (setf (point-amount spline)
          (1+ (floor (/ (aref (distances spline) (1- (length (distances spline))))
                          point-distance)))))
  (calculate-cubic-spline-interpolation spline :from *cubic-spline-from-hack*))

(defun calculate-cubic-spline-distances (spline &key (from 0))
  (let* ((data (data spline))
         (distances (distances spline))
         (len (length (aref data 0))))
    (when (< 1 len)
      (ensure-length distances len)
      (let ((err NIL)
            (x-left 0.0) (y-left 0.0)
            (x-right 0.0) (y-right 0.0)
            (x-middle 0.0) (y-middle 0.0)
            (g 0.0) (dn 0.0))
        (declare (float x-left y-left x-right y-right x-middle y-middle g dn))
        (flet ((calculate-distance (i)
                 (if (= dn 0.0)
                     (setf g 1.0)
                     (let ((dz (+ (* x-right x-middle)
                                  (* y-right y-middle))))
                       (if (= dz 0.0)
                           (setf g (/ PI 2))
                           (setf dz (/ dz dn)
                                 g (* (sqrt (1+ (expt dz 2)))
                                      (atan (/ 1 (abs dz))))))))
                 (let ((distance (* g (sqrt (+ (expt x-left 2)
                                               (expt y-left 2))))))
                   (if (< distance 0.0)
                       (setf err T) ; somehow this happened.
                       (setf (aref distances i)
                             (+ (aref distances (1- i)) distance))))))
          ;; Calculate middle
          (loop for i from (1+ from) below (1- len)
                do (setf x-left (- (idata data 0 i)
                                   (idata data 0 (1- i)))
                         y-left (- (idata data 1 i)
                                   (idata data 1 (1- i)))
                         x-right (- (idata data 0 (1+ i))
                                    (idata data 0 i))
                         y-right (- (idata data 1 (1+ i))
                                    (idata data 1 i))
                         x-middle (- (idata data 0 (1+ i))
                                     (idata data 0 (1- i)))
                         y-middle (- (idata data 1 (1+ i))
                                     (idata data 1 (1- i)))
                         dn (- (* x-left y-right)
                               (* y-left x-right)))
                   (calculate-distance i)
                until err)
          ;; Calculate end point
          (unless err
            (setf g x-left
                  x-left (- x-right)
                  x-right (- g)
                  g y-left
                  y-left (- y-right)
                  y-right (- g)
                  x-middle (- x-middle)
                  y-middle (- y-middle)
                  dn (- (* x-left y-right)
                        (* y-left x-right)))
            (calculate-distance (1- len))))
        ;; Something screwed up. Fix it with a linear approximation.
        (when err
          (loop for i from (1+ from) below len
                do (let ((distance (sqrt (+ (expt (- (idata data 0 (- i 1))
                                                     (idata data 0 (- i 2))) 2)
                                            (expt (- (idata data 1 (- i 1))
                                                     (idata data 1 (- i 2))) 2)))))
                     (setf (aref distances i)
                           (+ (aref distances (1- i)) distance)))))))))

(defun calculate-cubic-spline-data (spline &key (from 0))
  (let* ((data (data spline))
         (len (length (aref data 0)))
         (datacount (length data)))
    (when (< 1 len)
      (dotimes (j datacount)
        (let ((u (make-array len :element-type 'float :initial-element 0.0))
              (c (aref (spline-data spline) j))
              (distances (distances spline))
              (data (aref (data spline) j)))
          (ensure-length c len)
          (loop for i from (1+ from) below (1- len)
                do (let* ((q (/ (- (aref distances i)
                                   (aref distances (1- i)))
                                (- (aref distances (1+ i))
                                   (aref distances (1- i)))))
                          (p (+ 2 (* q (aref c i)))))
                     (setf (aref c i) (/ (1- q)
                                         p)
                           (aref u i) (/ (- (/ (* 6 (- (/ (- (aref data (1+ i))
                                                             (aref data i))
                                                          (- (aref distances (1+ i))
                                                             (aref distances i)))
                                                       (/ (- (aref data i)
                                                             (aref data (1- i)))
                                                          (- (aref distances i)
                                                             (aref distances (1- i))))))
                                               (- (aref distances (1+ i))
                                                  (aref distances (1- i))))
                                            (* q (aref u (1- i))))
                                         p))))
          (setf (aref c (1- len)) 0.0)
          (loop for i downfrom (- len 2) to from
                do (setf (aref c i)
                         (+ (* (aref c i)
                               (aref c (1+ i)))
                            (aref u i)))))))))

(defun interpolate-cubic-spline (spline x c distance)
  (let ((distances (distances spline)))
    (if (< (length distances) 2)
        0.0
        (let ((klow 1)
              (khigh (length distances)))

          (loop while (< 1 (- khigh klow))
                do (let ((k (floor (/ (+ khigh klow) 2))))
                     (if (< distance (aref distances (1- k)))
                         (setf khigh k)
                         (setf klow k))))
          (decf khigh)
          (decf klow)
          (let ((dx (- (aref distances khigh)
                       (aref distances klow))))
            (if (= 0 dx)
                (/ (+ (aref x klow) (aref x khigh))
                   2)
                (let ((ai (/ (- (aref distances khigh) distance)
                             dx))
                      (bi (/ (- distance (aref distances klow))
                             dx)))
                  (+ (* ai (aref x klow))
                     (* bi (aref x khigh))
                     (/ (* (+ (* (- (expt ai 3) ai)
                                 (aref c klow))
                              (* (- (expt bi 3) bi)
                                 (aref c khigh)))
                           dx dx)
                        6)))))))))

(defun calculate-cubic-spline-interpolation (spline &key (from 0))
  (let ((len (length (aref (data spline) 0)))
        (point-amount (point-amount spline))
        (point-distance (point-distance spline)))
    (when (< 1 len)
      (loop for data across (data spline)
            for spline-data across (spline-data spline)
            for inter across (interpolated spline)
            do (ensure-length inter point-amount)
               (loop for j from (* from (floor (/ point-amount len))) below point-amount
                     do 
                        (setf (aref inter j)
                              (interpolate-cubic-spline spline data spline-data (* j point-distance))))))))

(defgeneric add-data-point (spline x y &rest additional-data)
  (:method ((spline cubic) x y &rest additional-data)
    (let ((new-data (cons x (cons y additional-data)))
          (len (length (aref (data spline) 0))))
      (unless (= (length new-data) (length (data spline)))
        (error "Wrong amount of data points! Required: ~d" (length (data spline))))
      (when (or (= 0 len)
                (/= x (idata (data spline) 0 (1- len)))
                (/= y (idata (data spline) 1 (1- len))))
        (loop for data in new-data
              for i from 0
              do (vector-push-extend (float data) (elt (data spline) i)))
        
        ;; recalculate three last
        (let ((newpos (- (length (aref (data spline) 0)) 3)))
          (when (<= 0 newpos)
            (calculate-cubic-spline-distances spline :from newpos)
            (calculate-cubic-spline-data spline :from newpos)
            (let ((*cubic-spline-from-hack* newpos))
              (setf (point-distance spline) (point-distance spline)))))))
    spline))

(defun make-cubic-spline (x y &rest additional-data)
  (let ((spline (make-instance 'cubic)))
    (let ((len (length x)))
      (loop for data in (cons x (cons y additional-data))
            do (when (/= len (length data))
                 (error "Not all arrays are the right size!"))
               (vector-push-extend (copy-adjustable data) (data spline))
               (vector-push-extend (make-array len :element-type 'float :initial-element 0.0 :adjustable T :fill-pointer T) (spline-data spline))
               (vector-push-extend (make-array len :element-type 'float :initial-element 0.0 :adjustable T :fill-pointer T) (interpolated spline)))
      (setf (distances spline)     (make-array len :element-type 'float :initial-element 0.0 :adjustable T :fill-pointer T)))
    (calculate-cubic-spline-distances spline)
    (calculate-cubic-spline-data spline)
    (setf (point-distance spline) 2)
    spline))

(defun make-empty-cubic-spline (&optional (field-count 2))
  (when (< field-count 2)
    (error "FIELD-COUNT must be >=2."))
  (apply #'make-cubic-spline (loop repeat field-count collect #1A())))

(defmethod make-curve ((type (eql 'cubic)))
  (make-empty-cubic-spline 5))

(defmethod record-point ((spline cubic) x y x-tilt y-tilt pressure)
  (add-data-point spline x y x-tilt y-tilt pressure))

(defmethod point-data ((spline cubic) pos)
  (list (aref (aref (interpolated spline) 0) pos)
        (aref (aref (interpolated spline) 1) pos)
        (aref (aref (interpolated spline) 2) pos)
        (aref (aref (interpolated spline) 3) pos)
        (aref (aref (interpolated spline) 4) pos)))

(defmethod map-points ((spline cubic) function &key from to)
  (unless from (setf from 0))
  (unless to (setf to (point-amount spline)))
  (let ((xs (aref (interpolated spline) 0))
        (ys (aref (interpolated spline) 1))
        (xts (aref (interpolated spline) 2))
        (yts (aref (interpolated spline) 3))
        (ps (aref (interpolated spline) 4)))
    (loop for i from from below to
          do (funcall function (aref xs i) (aref ys i) (aref xts i) (aref yts i) (aref ps i)))))