#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)

(defclass queue ()
  ((head :initform () :accessor head)
   (tail :initform () :accessor tail)))

(defun empty (queue)
  (setf (head queue) NIL
        (tail queue) NIL))

(defun empty-p (queue)
  (null (head queue)))

(defun push-item (queue item)
  (setf (tail queue)
        (if (empty-p queue)
            (setf (head queue)
                  (cons item NIL))
            (setf (cdr (tail queue))
                  (cons item NIL))))
  (head queue))

(defun pop-item (queue)
  (pop (head queue)))

(defun items (queue)
  (copy-list (head queue)))

(defmacro do-items ((item queue &optional return) &body body)
  `(dolist (,item (head ,queue) ,return)
     ,@body))

(defun map-items (queue function)
  (do-items (item queue)
    (funcall function item)))
