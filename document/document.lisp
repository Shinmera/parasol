#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass document ()
  ((%canvas :accessor canvas)
   (%pressed :initform NIL :accessor pressed)
   (%name :initarg :name :initform "Untitled" :accessor name))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override ("paintEvent" paint-event)
             ("tabletEvent" tablet-event)))

(defmethod initialize-instance :after ((document document) &key)
  (new document)
  (setf (canvas document) (make-instance 'canvas :document document)))

(defmethod tablet-event ((widget document) event)
  (qtenumcase (#_type event)
    ((#_QEvent::TabletPress)
     (unless (pressed widget)
       (setf (pressed widget) T)
       (start-stroke (canvas widget)
                     (#_x event) (#_y event)
                     (#_xTilt event) (#_yTilt event) (#_pressure event))))
    ((#_QEvent::TabletRelease)
     (setf (pressed widget) NIL))
    ((#_QEvent::TabletMove)
     (when (pressed widget)
       (record-point (canvas widget)
                     (#_x event) (#_y event)
                     (#_xTilt event) (#_yTilt event) (#_pressure event)))))
  (#_update widget))

(defmethod paint-event ((widget document) event)
  (with-objects ((painter (#_new QPainter widget)))
    (draw (canvas widget) painter)
    (#_end painter)))



