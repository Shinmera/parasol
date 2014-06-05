#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass canvas-widget()
  ((%pressed :initform NIL :accessor pressed)
   (%canvas :initform (make-instance 'canvas) :accessor canvas)
   (%background :initform (#_new QColor 120 120 120) :accessor background)

   (%offset-x :initform 0 :accessor offset-x)
   (%offset-y :initform 0 :accessor offset-y))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override ("paintEvent" paint-event)
             ("resizeEvent" resize-event)
             ("tabletEvent" tablet-event)))

(defmethod initialize-instance :after ((canvas-widget canvas-widget) &key)
  (new canvas-widget)
  (#_setAutoFillBackground canvas-widget T))

(defmethod resize-event ((widget canvas-widget) event)
  (setf (offset-x widget) (floor (/ (- (#_width widget) (width (canvas widget))) 2))
        (offset-y widget) (floor (/ (- (#_height widget) (height (canvas widget))) 2))))

(defmethod tablet-event ((widget canvas-widget) event)
  (qtenumcase (#_type event)
    ((#_QEvent::TabletPress)
     (unless (pressed widget)
       (setf (pressed widget) T)
       (start-stroke (canvas widget)
                     (- (#_x event) (offset-x widget))
                     (- (#_y event) (offset-y widget))
                     (#_xTilt event) (#_yTilt event) (#_pressure event))))
    ((#_QEvent::TabletRelease)
     (setf (pressed widget) NIL))
    ((#_QEvent::TabletMove)
     (when (pressed widget)
       (record-point (canvas widget)
                     (- (#_x event) (offset-x widget))
                     (- (#_y event) (offset-y widget))
                     (#_xTilt event) (#_yTilt event) (#_pressure event)))))
  (#_update widget))

(defmethod paint-event ((widget canvas-widget) event)
  (let ((painter (#_new QPainter widget)))
    (#_fillRect painter 0 0 (#_width widget) (#_height widget) (background widget))
    (#_drawPixmap painter
                  (offset-x widget)
                  (offset-y widget)
                  (pixmap (canvas widget)))
    (#_end painter)))
