#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.document)

(define-finalizable history-item ()
  ((document :initarg :document :initform (error "Document required.") :reader document)
   (applied :initarg :applied :initform NIL :accessor applied)))

(defgeneric undo (history-item)
  (:method :around ((item history-item))
    (unless (applied item)
      (error "~s has not yet been applied." item))
    (v:info :history "Undoing ~s" item)
    (call-next-method)
    (setf (applied item) NIL)
    item))

(defgeneric redo (history-item)
  (:method :around ((item history-item))
    (when (applied item)
      (error "~s has already been applied." item))
    (v:info :history "Redoing ~s" item)
    (call-next-method)
    (setf (applied item) T)
    item))

(define-finalizable history ()
  ((items :initarg :items :initform (make-array 0 :adjustable T :fill-pointer 0) :accessor items :finalized T)
   (age :initarg :age :initform NIL :accessor age)))

(defmethod initialize-instance :after ((history history) &key)
  (unless (age history)
    (setf (age history) (length (items history)))))

;; These two could be much more concise, but I like
;; the clarity.
(defgeneric record (item history)
  (:method ((item history-item) (history history))
    (with-slots (items age) history
      (redo item)
      ;; Since we might be some time back in the history
      ;; we need to set the fill-pointer to our current
      ;; point (thus clearing the rest.
      (setf (fill-pointer items) age)
      (incf age)
      (vector-push-extend item items))
    item))

(defgeneric rewind (history)
  (:method ((history history))
    (with-slots (items age) history
      (let ((item (aref items (1- age))))
        (undo item)
        (vector-pop items)
        (decf age)
        item))))

(defgeneric size (history)
  (:method ((history history))
    (length (items history))))

(defgeneric current-history (history)
  (:method ((history history))
    (subseq (items history) 0 (age history))))

(define-finalizable function-call-history-item (history-item)
  ((undo-func :initarg :undo :initform (error "Undo required.") :accessor undo-func)
   (redo-func :initarg :redo :initform (error "Redo required.") :accessor redo-func)))

(defmethod undo ((item function-call-history-item))
  (funcall (undo-func item)))

(defmethod redo ((item function-call-history-item))
  (funcall (redo-func item)))

(define-finalizable slot-change-history-item (history-item)
  ((object :initarg :object :initform (error "Object required.") :accessor object)
   (slot :initarg :slot :initform (error "Slot required.") :accessor slot)
   (original :initarg :original :initform NIL :accessor original)
   (value :initarg :value :initform (error "Value required.") :accessor value)))

(defmethod redo ((item slot-change-history-item))
  (setf (original item) (slot-value (object item) (slot item))
        (slot-value (object item) (slot item)) (value item)))

(defmethod undo ((item slot-change-history-item))
  (setf (slot-value (object item) (slot item)) (original item)))

(define-finalizable vector-push-history-item (history-item)
  ((change-vector :initarg :vector :initform (error "Vector required.") :accessor change-vector)
   (item :initarg :item :initform (error "Item required.") :accessor item)))

(defmethod redo ((item vector-push-history-item))
  (vector-push-extend (item item) (change-vector item)))

(defmethod undo ((item vector-push-history-item))
  (let ((return (vector-pop (change-vector item))))
    (unless (eql return (item item))
      (error "WTF? Undoing of ~s resulted in unexpected element ~s being popped. Expected ~s."
             item return (item item)))))
