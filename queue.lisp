#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)

(defstruct queue
  (tail NIL)
  (head NIL))

(defstruct qcons
  (item NIL)
  (next NIL)
  (prev NIL))

(defmethod print-object ((qcons qcons) stream)
  (print-unreadable-object (qcons stream :type T)
    (format stream "~a" (qcons-item qcons))))

(defun queue-push (item queue)
  (let ((cell (make-qcons :item item :next (queue-tail queue) :prev NIL)))
    (if (queue-tail queue)
        (setf (qcons-prev (queue-tail queue)) cell)
        (setf (queue-head queue) cell))
    (setf (queue-tail queue) cell))
  queue)

(defun queue-pop (queue)
  (let ((cell (queue-head queue)))
    (when cell
      (setf (queue-head queue) (qcons-prev cell))
      (if (queue-head queue)
          (setf (qcons-next (queue-head queue)) NIL)
          (setf (queue-tail queue) NIL))
      (qcons-item cell))))

(defun queue-list (queue)
  (loop for qcons = (queue-head queue)
          then (qcons-prev qcons)
        while qcons
        collect (qcons-item qcons)))

(defun queue-clear (queue)
  (setf (queue-tail queue) NIL
        (queue-head queue) NIL))
