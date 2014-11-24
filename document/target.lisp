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

(defun (setf target-backend) (backend)
  (setf *target-backend* (find-symbol (string backend) "KEYWORD")))

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
  (let* ((image (#_new QImage (uiop:native-namestring pathname)))
         (target (make-target (#_width image) (#_height image))))
    (with-painter (painter target)
      (#_drawImage painter 0 0 image))
    (finalize image)
    target))

(defgeneric to-image (target)
  (:method ((target target))
    (error "TO-IMAGE of the target ~s is not implemented." target)))

(defgeneric painter (target)
  (:method ((widget widget))
    (#_new QPainter widget))
  (:method ((object qobject))
    (#_new QPainter object)))

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
    (let ((image (#_new QImage (width target) (height target) (#_QImage::Format_ARGB32))))
      (#_fill image (#_Qt::transparent))
      (setf (image target) image))))

(defmethod to-image ((target qimage-target))
  (image target))

(defmethod copy ((target qimage-target))
  (make-instance 'qimage-target :width (width target) :height (height target)
                                :image (copy (image target))))

(defmethod clear ((target qimage-target) &optional (color (#_Qt::transparent)))
  (#_fill (image target) color)
  target)

(defmethod draw ((target qimage-target) painter)
  (#_drawImage painter 0 0 (image target))
  target)

(defmethod fit ((target qimage-target) width height &key (x 0) (y 0))
  (unless (and (= (width target) width)
               (= (height target) height))
    (let ((new (#_new QImage width height (#_QImage::Format_ARGB32))))
      (#_fill new (#_Qt::transparent))
      (with-finalizing ((painter (#_new QPainter new)))
        (#_drawImage painter x y (image target)))
      (setf (image target) new)))
  target)
