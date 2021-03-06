#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.document)
(named-readtables:in-readtable :qtools)

(define-finalizable meta-layer ()
  ((drawables :initform (make-array 20 :element-type 'drawable :adjustable T :fill-pointer 0) :reader drawables :finalized T)
   (current-index :initform NIL :accessor current-index)))

(defmethod (setf current-index) :before (index (layer meta-layer))
  (v:info :layer "[~a] Switching current drawable to ~a(~a)"
          layer (drawable-at index layer) index))

(defgeneric current-drawable (layer)
  (:method ((layer meta-layer))
    (and (current-index layer)
         (aref (drawables layer) (current-index layer)))))

(defgeneric (setf current-drawable) (drawable layer)
  (:method ((drawable drawable) (layer meta-layer))
    (let ((pos (position drawable (drawables layer))))
      (if pos
          (setf (current-index layer) pos)
          (error "Drawable ~a is not contained in ~a" drawable layer)))))

(defgeneric insert (drawable layer &optional position)
  (:method ((drawable drawable) (layer meta-layer) &optional position)
    (v:info :meta-layer "[~a] Inserting drawable ~a" layer drawable)
    (cond
      (position
       (vector-push-extend-position drawable (drawables layer) position))
      (T
       (vector-push-extend drawable (drawables layer))
       (activate drawable layer)))
    drawable))

(defgeneric extract (drawable layer)
  (:method ((drawable drawable) (layer meta-layer))
    (extract (find drawable (drawables layer)) layer))

  (:method ((index fixnum) (layer meta-layer))
    (v:info :meta-layer "[~a] Removing drawable ~a" layer index)
    (vector-pop-position (drawables layer) index)
    (when (= index (current-index layer))
      (setf (current-index layer) 0))))

(defgeneric drawable-at (index layer)
  (:method ((index fixnum) (layer meta-layer))
    (aref (drawables layer) index)))

(defgeneric activate (drawable layer)
  (:method ((drawable drawable) (layer meta-layer))
    (setf (current-drawable layer) drawable))

  (:method ((index fixnum) (layer meta-layer))
    (setf (current-index layer) index)))

(defgeneric size (layer)
  (:method ((layer meta-layer))
    (length (drawables layer))))

(define-finalizable layer (meta-layer buffered metadata)
  ((opacity :initarg :opacity :initform 1.0 :accessor opacity)
   (mode :initarg :mode :initform 0 #|source-over|# :accessor mode)
   (visible :initarg :visible :initform T :accessor visible)))

;; (defmethod draw :around ((layer layer) target)
;;   (when (visible layer)
;;     (setf (q+:composition-mode target) (mode layer))
;;     (setf (q+:opacity target) (opacity layer))
;;     (call-next-method)))

;; (defmethod draw-buffer ((layer layer) target)
;;   (loop for drawable across (drawables layer)
;;         do (draw drawable target)))

(defmethod draw ((layer layer) target)
  (when (visible layer)
    (setf (q+:composition-mode target) (mode layer))
    (setf (q+:opacity target) (opacity layer))
    (loop for drawable across (drawables layer)
          do (draw drawable target))))
