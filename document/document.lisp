#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defvar *mouse-pressure* 0.5)

(defclass document ()
  ((%canvas :accessor canvas)
   (%pressed :initform NIL :accessor pressed)
   (%name :initarg :name :initform "Untitled" :accessor name))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override ("paintEvent" paint-event)
             ("tabletEvent" tablet-event)
             ("mousePressEvent" mouse-press-event)
             ("mouseReleaseEvent" mouse-release-event)
             ("mouseMoveEvent" mouse-move-event)))

(defmethod initialize-instance :after ((document document) &key)
  (new document)
  (setf (canvas document) (make-instance 'canvas :document document)))

(defmethod tablet-event ((widget document) event)
  (qtenumcase (#_type event)
    ((#_QEvent::TabletPress)
     (unless (pressed widget)
       (setf (pressed widget) T)
       (start-stroke (canvas widget)
                     (enum-value (#_pointerType event))
                     (#_x event) (#_y event)
                     (#_xTilt event) (#_yTilt event) (#_pressure event))
       (#_update widget)))
    ((#_QEvent::TabletRelease)
     (setf (pressed widget) NIL))
    ((#_QEvent::TabletMove)
     (when (pressed widget)
       (record-point (canvas widget)
                     (#_x event) (#_y event)
                     (#_xTilt event) (#_yTilt event) (#_pressure event))
       (#_update widget)))))

(defmethod mouse-press-event ((widget document) event)
  (unless (pressed widget)
    (setf (pressed widget) T)
    (start-stroke (canvas widget)
                  2 (#_x event) (#_y event)
                  0 0 *mouse-pressure*)
    (#_update widget)))

(defmethod mouse-move-event ((widget document) event)
  (when (pressed widget)
    (record-point (canvas widget)
                  (#_x event) (#_y event)
                  0 0 *mouse-pressure*)
    (#_update widget)))

(defmethod mouse-release-event ((widget document) event)
  (setf (pressed widget) NIL))

(defmethod paint-event ((widget document) event)
  (with-objects ((painter (#_new QPainter widget)))
    (draw (canvas widget) painter)
    (#_end painter)))

(defmethod make-active ((widget document))
  )

(defmethod destroy ((widget document))
  )
