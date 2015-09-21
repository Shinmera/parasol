#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.document)
(named-readtables:in-readtable :qtools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *target-backend* :qimage))

(defun target-backend ()
  *target-backend*)

(defgeneric (setf target-backend) (backend)
  (:method (backend)
    (v:info :parasol "Switching target backend to ~a" backend)
    (setf *target-backend* (find-symbol (string backend) "KEYWORD"))))

(define-finalizable target ()
  ((width :initarg :width :initform (error "WIDTH required.") :accessor width)
   (height :initarg :height :initform (error "HEIGHT required.") :accessor height)
   (painter :initform NIL :accessor painter)))

(defmethod print-object ((target target) stream)
  (print-unreadable-object (target stream :type T :identity T)
    (format stream "~ax~a" (width target) (height target)))
  target)

(defun make-target (width height &optional (backend *target-backend*))
  (make-target-instance backend width height))

(defgeneric make-target-instance (backend width height)
  (:method (backend width height)
    (make-instance backend :width width :height height)))

(defun make-target-from (pathname)
  (let* ((image (q+:make-qimage (uiop:native-namestring pathname)))
         (target (make-target (q+:width image) (q+:height image))))
    (with-painter (painter target)
      (q+:draw-image painter 0 0 image))
    (finalize image)
    target))

(defgeneric to-image (target)
  (:method ((target target))
    (error "TO-IMAGE of the target ~s is not implemented." target)))

(defgeneric painter (target)
  (:method ((widget widget))
    (q+:make-qpainter widget))
  (:method ((object qobject))
    (q+:make-qpainter object)))

(defmethod copy ((target target))
  (error "COPY of the target ~s is not implemented." target))

(defgeneric clear (target &optional color)
  (:method ((target target) &optional color)
    (declare (ignore color))
    (error "CLEAR of the target ~s is not implemented." target)))

(defmethod draw ((target target) painter)
  (error "DRAW of the target ~s is not implemented." target))

(defgeneric fit (target width height &key x y)
  (:method ((target target) width height &key x y)
    (declare (ignore width height x y))
    (error "FIT of the target ~s is not implemented." target))
  (:method :after ((target target) width height &key x y)
    (declare (ignore x y))
    (setf (width target) width
          (height target) height)))

;; QImage impl.
(defmethod make-target-instance ((class (eql :qimage)) width height)
  (make-instance 'qimage-target :width width :height height))

(define-finalizable qimage-target (target)
  ((image :initarg :image :initform NIL :accessor image :finalized T)))

(defmethod (setf image) :around (value (target qimage-target))
  (unless (eql value (image target))
    (when (painter target)
      (finalize (painter target)))
    (when (image target)
      (finalize (image target)))
    (call-next-method)
    (setf (painter target)
          (make-painter value))))

(defmethod initialize-instance :after ((target qimage-target) &key)
  (unless (image target)
    (let ((image (q+:make-qimage (width target) (height target) (q+:qimage.format_argb32))))
      (q+:fill image (q+:qt.transparent))
      (setf (image target) image))))

(defmethod to-image ((target qimage-target))
  (image target))

(defmethod copy ((target qimage-target))
  (make-instance 'qimage-target :width (width target) :height (height target)
                                :image (copy (image target))))

(defmethod clear ((target qimage-target) &optional (color (q+:qt.transparent)))
  (q+:fill (image target) color)
  target)

(defmethod draw ((target qimage-target) painter)
  (q+:draw-image painter 0 0 (image target))
  target)

(defmethod fit ((target qimage-target) width height &key (x 0) (y 0))
  (unless (and (= (width target) width)
               (= (height target) height))
    (let ((new (q+:make-qimage width height (q+:qimage.format_argb32))))
      (q+:fill new (q+:qt.transparent))
      (with-finalizing ((painter (q+:make-qpainter new)))
        (q+:draw-image painter x y (image target)))
      (setf (image target) new)))
  target)
