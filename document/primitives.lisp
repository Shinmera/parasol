#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.document)
(named-readtables:in-readtable :qtools)

(defvar *mode-map* '((:source-over . 0)
                     (:destination-over . 1)
                     (:clear . 2)
                     (:source . 3)
                     (:destination . 4)
                     (:source-in . 5)
                     (:destination-in . 6)
                     (:source-out . 7)
                     (:destination-out . 8)
                     (:source-atop . 9)
                     (:destination-atop . 10)
                     (:xor . 11)
                     (:plus . 12)
                     (:multiply . 13)
                     (:screen . 14)
                     (:overlay . 15)
                     (:darken . 16)
                     (:lighten . 17)
                     (:color-dodge . 18)
                     (:color-burn . 19)
                     (:hard-light . 20)
                     (:soft-light . 21)
                     (:difference . 22)
                     (:exclusion . 23)
                     (:source-or-destination . 24)
                     (:source-and-destination . 25)
                     (:source-xor-destination . 26)
                     (:not-source-and-not-destination . 27)
                     (:not-source-or-not-destination . 28)
                     (:not-source-xor-destination . 29)
                     (:not-source . 30)
                     (:not-source-and-destination . 31)
                     (:source-and-not-destination . 32)))

(defun to-mode-num (mode-name)
  (cdr (find (if (keywordp mode-name) mode-name (find-symbol (string-upcase mode-name) "KEYWORD")) *mode-map* :key #'car)))

(defun to-mode-name (mode-num)
  (car (find mode-num *mode-map* :key #'cdr)))

(define-finalizable positioned ()
  ((x :initarg :x :initform 0.0 :accessor x)
   (y :initarg :y :initform 0.0 :accessor y)))

(defmethod print-object ((pos positioned) stream)
  (print-unreadable-object (pos stream :type T :identity T)
    (format stream "~a/~a" (x pos) (y pos)))
  pos)

(defgeneric translate-to (positioned transform)
  (:method ((pos positioned) (transform qobject))
    (q+:translate transform (q+:make-qpointf (x pos) (y pos)))))

(defgeneric translate-away (positioned transform)
  (:method ((pos positioned) (transform qobject))
    (q+:translate transform (q+:make-qpointf (- (x pos)) (- (y pos))))))

(defmacro with-transformation ((painter) &body body)
  (let ((paint (gensym "PAINTER")))
    `(let* ((,paint ,painter))
       (unwind-protect
            (progn
              (q+:save ,paint)
              ,@body)
         (q+:restore ,paint)))))

(defmacro with-translation-to ((positioned transform) &body body)
  (let ((pos (gensym "POSITIONED"))
        (tr (gensym "TRANSFORM")))
    `(let ((,pos ,positioned) (,tr ,transform))
       (translate-to ,pos ,tr)
       (unwind-protect
            (progn ,@body)
         (translate-away ,pos ,tr)))))

(defmacro with-translation-away ((positioned transform) &body body)
  (let ((pos (gensym "POSITIONED"))
        (tr (gensym "TRANSFORM")))
    `(let ((,pos ,positioned) (,tr ,transform))
       (translate-away ,pos ,tr)
       (unwind-protect
            (progn ,@body)
         (translate-to ,pos ,tr)))))

(define-finalizable drawable (positioned)
  ((width :initarg :width :initform 0.0 :accessor width)
   (height :initarg :height :initform 0.0 :accessor height)))

(defgeneric draw (drawable target)
  (:method :around ((drawable drawable) target)
    (with-translation-to (drawable target)
      (call-next-method))))

(define-finalizable buffered (drawable)
  ((buffer :initarg :buffer :initform NIL :accessor buffer :finalized T)))

(defmacro define-delegated-accessor (name from-class accessor)
  (let ((value (gensym "VALUE")))
    `(progn
       (defmethod ,name ((,from-class ,from-class))
         (,name (,accessor ,from-class)))
       (defmethod (setf ,name) (,value (,from-class ,from-class))
         (setf (,name (,accessor ,from-class)) ,value)))))

(define-delegated-accessor width buffered buffer)
(define-delegated-accessor height buffered buffer)

(defmethod (setf buffer) :before (value (buffered buffered))
  (unless (eql (buffer buffered) value)
    (finalize (buffer buffered))))

(defmethod painter ((buffered buffered))
  (painter (buffer buffered)))

(defgeneric draw-buffer (buffered target)
  (:method :around ((buffered buffered) target)
    (with-translation-away (buffered target)
      (call-next-method))))

(defgeneric rebuffer (buffered)
  (:method ((buffered buffered))
    (let* ((buffer (buffer buffered))
           (painter (painter buffer)))
      (q+:erase-rect painter 0 0 (width buffer) (height buffer))
      (draw-buffer buffered painter))))

(defgeneric rebuffer-copy (buffered)
  (:method ((buffered buffered))
    (let* ((old (buffer buffered))
           (new (make-target (width old) (height old))))
      (with-finalizing ((painter (make-painter new)))
        (draw-buffer buffered painter))
      (setf (buffer buffered) new))))

(defmethod draw ((buffered buffered) target)
  (when (buffer buffered)
    (draw (buffer buffered) target)))

(define-finalizable adaptive-buffered (buffered positioned)
  ((chunk-size :initarg :chunk-size :initform 200 :accessor chunk-size)
   (initial-size :initarg :initial-size :initform 500 :accessor initial-size)))

(defmethod initialize-instance :after ((buffered adaptive-buffered) &key)
  (setf (buffer buffered)
        (make-target (initial-size buffered)
                     (initial-size buffered)))
  ;; To avoid issues when extending on some targets we need
  ;; to floor it to an int here.
  (decf (x buffered) (floor (/ (initial-size buffered) 2)))
  (decf (y buffered) (floor (/ (initial-size buffered) 2))))

(defun ensure-range (n range &key (expansion range))
  (flet ((expand-to (n)
           (* expansion (ceiling (/ (abs n) expansion)))))
    (let ((offset 0))
      (cond
        ((<= range (+ n (/ expansion 2)))
         (setf range (expand-to (+ n (/ expansion 2)))))

        ((<= n (/ expansion 2))
         (setf offset (expand-to (- n (/ expansion 2))))
         (setf range (+ range offset))))
      (values range (- offset)))))

(defgeneric ensure-fitting (x y layer)
  (:method (x y (buffered adaptive-buffered))
    (let ((buffer (buffer buffered)))
      (multiple-value-bind (width xd) (ensure-range (- x (x buffered)) (width buffer) :expansion (chunk-size buffered))
        (multiple-value-bind (height yd) (ensure-range (- y (y buffered)) (height buffer) :expansion (chunk-size buffered))
          (setf (buffer buffered) (fit buffer width height :x (- xd) :y (- yd)))
          (incf (x buffered) xd)
          (incf (y buffered) yd))))
    buffered))

(defclass color ()
  ((r :initarg :r :accessor r)
   (g :initarg :g :accessor g)
   (b :initarg :b :accessor b)
   (a :initarg :a :accessor a))
  (:default-initargs
   :r 0 :g 0 :b 0 :a 255))

(defun to-qcolor (color)
  (q+:make-qcolor (r color) (g color) (b color) (a color)))
