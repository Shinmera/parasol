#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defvar *mouse-pressure* 0.5)

;; Apparently Qt reuses tablet event objects, so since we need to pass it
;; on to the mouse event for later, we need to save its data here. Gr8.
(defclass tablet-event ()
  ((%x :initarg :x :accessor x)
   (%y :initarg :y :accessor y)
   (%x-tilt :initarg :x-tilt :accessor x-tilt)
   (%y-tilt :initarg :y-tilt :accessor y-tilt)
   (%pressure :initarg :pressure :accessor pressure)
   (%pointer :initarg :pointer :accessor pointer)))

(defclass document ()
  ((%canvas :accessor canvas)
   (%mode :initform NIL :accessor mode)
   (%tab-event :initform NIL :accessor tab-event)
   (%name :initarg :name :initform "Untitled" :accessor name)
   (%last-mouse :initform '(0 . 0) :accessor last-mouse))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override ("paintEvent" paint-event)
             ("tabletEvent" tablet-event)
             ("mousePressEvent" mouse-press-event)
             ("mouseReleaseEvent" mouse-release-event)
             ("mouseMoveEvent" mouse-move-event)))

(defmethod print-object ((document document) stream)
  (print-unreadable-object (document stream :type T :identity T)
    (format stream "~a" (name document)))
  document)

(defmethod initialize-instance :after ((document document) &key)
  (new document)
  (setf (canvas document) (make-instance 'canvas :document document)))

(defmethod tablet-event ((widget document) event)
  (if (enum= (#_type event) (#_QEvent::TabletRelease))
      (setf (tab-event widget) NIL)
      (setf (tab-event widget)
            (make-instance
             'tablet-event
             :x (#_x event) :y (#_y event)
             :x-tilt (#_xTilt event) :y-tilt (#_yTilt event)
             :pressure (#_pressure event)
             :pointer (enum-value (#_pointerType event)))))
  (#_ignore event))

(defmethod mouse-press-event ((widget document) event)
  (qtenumcase (#_button event)
    ((#_Qt::LeftButton)
     (if (tab-event widget)
         (progn
           (setf (mode widget) :tablet)
           (let ((event (tab-event widget)))
             (start-stroke (canvas widget)
                           (pointer event)
                           (x event) (y event)
                           (x-tilt event) (y-tilt event)
                           (pressure event))))
         (progn
           (setf (mode widget) :mouse)
           (start-stroke (canvas widget)
                         2 (#_x event) (#_y event)
                         0 0 *mouse-pressure*))))
    ((#_Qt::RightButton)
     (cycle-color *window*))
    ((#_Qt::MiddleButton)
     (setf (mode widget) :move)
     (setf (car (last-mouse widget)) (#_x event)
           (cdr (last-mouse widget)) (#_y event))))
  (#_update widget))

(defmethod mouse-move-event ((widget document) event)
  (case (mode widget)
    (:tablet
     (let ((event (tab-event widget)))
       (record-point (canvas widget)
                     (x event) (y event)
                     (x-tilt event) (y-tilt event)
                     (pressure event)))
     (#_update widget))
    (:mouse
     (record-point (canvas widget)
                   (#_x event) (#_y event)
                   0 0 *mouse-pressure*)
     (#_update widget))
    (:move
     (let ((last (last-mouse widget)))
       (move (canvas widget)
             (- (#_x event) (car last))
             (- (#_y event) (cdr last)))
       (setf (car last) (#_x event)
             (cdr last) (#_y event)))
     (#_update widget))))

(defmethod mouse-release-event ((widget document) event)
  (case (mode widget)
    ((:tablet :mouse)
     (end-stroke (canvas widget))
     (#_update widget)))
  (setf (mode widget) NIL
        (tab-event widget) NIL))

(defmethod paint-event ((widget document) event)
  (with-objects ((painter (#_new QPainter widget)))
    (draw (canvas widget) painter)
    (#_end painter)))

(defmethod make-active ((widget document))
  )

(defmethod finalize ((widget document))
  (finalize (canvas widget)))

(defmethod destroy ((widget document))
  ;; add checks
  )

;; delegate
(defmethod layers ((widget document))
  (layers (canvas widget)))

(defmethod add-layer ((widget document) &key name mode)
  (add-layer (canvas widget) :name name :mode mode))

(defmethod remove-layer ((widget document) &optional index)
  (remove-layer (canvas widget) index))

(defmethod (setf active-layer) (index (widget document))
  (setf (active-layer (canvas widget)) index))

(defmethod move-layer ((widget document) index)
  (move-layer (canvas widget) index))

(defmethod undo ((widget document))
  (undo (canvas widget)))

(defmethod redo ((widget document))
  (redo (canvas widget)))
