#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass color-hsv-widget ()
  ((%color :initform (#_new QColor 0 0 0) :accessor color)
   (%pressed :initform NIL :accessor pressed)
   (%size :initform 0 :accessor size)
   (%pixmap :initform NIL :accessor pixmap)
   (%parent :initarg :parent :initform (error "Parent required") :accessor parent))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override ("paintEvent" paint-event)
             ("resizeEvent" resize-event)
             ("mousePressEvent" mouse-press-event)
             ("mouseReleaseEvent" mouse-release-event)
             ("mouseMoveEvent" mouse-move-event)))

(defmethod initialize-instance :after ((widget color-hsv-widget) &key)
  (new widget)
  (update-color-triangle widget))

(defmethod update-color-triangle ((widget color-hsv-widget))
  (when (and (pixmap widget)
             (not (qt:null-qobject-p (pixmap widget))))
    (optimized-delete (pixmap widget)))
  (let* ((color (color widget))
         (size (size widget))
         (radius (floor (/ size 2)))
         (wheel-width (floor (/ size 15)))
         (deg (- (/ (* (#_hsvHue color) PI) 180)))
         (pixmap (#_new QPixmap size size)))
    (with-objects ((painter (#_new QPainter pixmap))
                   (white (#_new QColor 255 255 255))
                   (black (#_new QColor 0 0 0)))
      (#_setRenderHint painter (#_QPainter::Antialiasing))
      (#_setRenderHint painter (#_QPainter::HighQualityAntialiasing))
      (#_fill pixmap black)
      
      ;; Draw hsv-gradient wheel
      (with-objects ((hsv-gradient (#_new QConicalGradient (floor radius) (floor radius) 0))
                     (c1 (#_new QColor 255 0 0))
                     (c2 (#_new QColor 255 255 0))
                     (c3 (#_new QColor 0 255 0))
                     (c4 (#_new QColor 0 255 255))
                     (c5 (#_new QColor 0 0 255))
                     (c6 (#_new QColor 255 0 255)))
        (#_setColorAt hsv-gradient 0.000 c1)
        (#_setColorAt hsv-gradient 0.166 c2)
        (#_setColorAt hsv-gradient 0.333 c3)
        (#_setColorAt hsv-gradient 0.500 c4)
        (#_setColorAt hsv-gradient 0.666 c5)
        (#_setColorAt hsv-gradient 0.833 c6)
        (#_setColorAt hsv-gradient 1.000 c1)
        (#_setBrush painter (#_new QBrush hsv-gradient))
        (#_drawEllipse painter 0 0 size size))
      ;; Clear inner
      (with-objects ((brush (#_new QBrush black)))
        (#_setBrush painter brush)
        (#_drawEllipse painter wheel-width wheel-width (- size (* wheel-width 2)) (- size (* wheel-width 2))))
      ;; Draw indicator
      (with-objects ((pen (#_new QPen)))
        (#_setWidthF pen 5.0)
        (#_setColor pen white)
        (#_setPen painter pen)
        (#_drawLine painter
                    (round (+ (* (cos deg) (- radius wheel-width)) radius))
                    (round (+ (* (sin deg) (- radius wheel-width)) radius))
                    (round (+ (* (cos deg) radius) radius))
                    (round (+ (* (sin deg) radius) radius))))
      ;; Draw square (@fixme triangle, positioning/size)
      (with-objects ((ef1-gradient (#_new QLinearGradient
                                          (floor (* radius 0.5)) (floor (* radius 0.5))
                                          (floor (* radius 1.5)) (floor (* radius 0.5))))
                     (ef2-gradient (#_new QLinearGradient
                                          (floor (* radius 0.5)) (floor (* radius 0.5))
                                          (floor (* radius 0.5)) (floor (* radius 1.5))))
                     (c1 (#_new QColor 0 0 0 255))
                     (c2 (#_new QColor 0 0 0 0))
                     (c3 (#_QColor::fromHsv (abs (#_hsvHue color)) 255 255 255))
                     (c4 (#_new QColor 255 255 255 255)))
        (#_setColorAt ef1-gradient 0.0 c1)
        (#_setColorAt ef1-gradient 1.0 c2)
        (#_setColorAt ef2-gradient 0.0 c3)
        (#_setColorAt ef2-gradient 1.0 c4)
        (with-objects ((brush (#_new QBrush ef2-gradient)))
          (#_fillRect painter (floor (* radius 0.5)) (floor (* radius 0.5)) radius radius brush))
        (with-objects ((brush (#_new QBrush ef1-gradient)))
          (#_fillRect painter (floor (* radius 0.5)) (floor (* radius 0.5)) radius radius brush)))
      ;; Draw pick
      (with-objects ((pen (#_new QPen))
                     (brush (#_new QBrush))
                     (point (#_new QPoint
                                   (floor (* radius (+ 0.5 (/ (#_value color) 255))))
                                   (floor (* radius (+ 1.5 (- (/ (#_hsvSaturation color) 255))))))))
        (#_setColor pen white)
        (#_setWidthF pen 2.0)
        (#_setPen painter pen)
        (#_setBrush painter brush)
        (#_drawEllipse painter point 5 5))
      (#_end painter))
    (setf (pixmap widget) pixmap)))

(defmethod resize-event ((widget color-hsv-widget) event)
  (setf (size widget) (min (#_width widget) (#_height widget)))
  (update-color-triangle widget))

(defmethod paint-event ((widget color-hsv-widget) event)
  (let ((painter (#_new QPainter widget)))
    (#_fillRect painter 0 0 (#_width widget) (#_height widget) (#_new QColor 0 0 0))
    (#_drawPixmap painter
                  (floor (/ (- (#_width widget) (size widget)) 2))
                  (floor (/ (- (#_height widget) (size widget)) 2))
                  (pixmap widget))
    (#_end painter)))

(defun abs-x-y-to-r-p (widget x y)
  (let* ((x (- x (/ (#_width widget) 2)))
         (y (- y (/ (#_height widget) 2)))
         (r (sqrt (+ (expt x 2)
                     (expt y 2))))
         (p (+ 360 (- (/ (* (atan y x) 180) PI)))))
    (values r p x y)))

(defmethod mouse-press-event ((widget color-hsv-widget) event)
  ;; This is all super dumb.
  (let* ((size (size widget))
         (radius (float (/ size 2)))
         (wheel-width (floor (/ size 15))))
    (multiple-value-bind (r p x y) (abs-x-y-to-r-p widget (#_x event) (#_y event))
      (cond
        ((< (- radius wheel-width) r radius)
         (setf (pressed widget) :wheel)
         (mouse-move-event widget event))
        ((and (< (abs x) (/ radius 2))
              (< (abs y) (/ radius 2)))
         (setf (pressed widget) :picker)
         (mouse-move-event widget event))))))

(defmethod mouse-release-event ((widget color-hsv-widget) event)
  (when (pressed widget)
    (setf (pressed widget) NIL)
    (color-widget-update (parent widget) (#_toRgb (color widget)))))

(defmethod mouse-move-event ((widget color-hsv-widget) event)
  (let* ((size (size widget))
         (radius (float (/ size 2)))
         (wheel-width (floor (/ size 15))))
    (multiple-value-bind (r p x y) (abs-x-y-to-r-p widget (#_x event) (#_y event))
      (case (pressed widget)
        (:wheel
         (#_setHsv (color widget)
                   (round p)
                   (#_saturation (color widget))
                   (#_value (color widget)))
         (update-color-triangle widget)
         (#_update widget))

        (:picker
         (#_setHsv (color widget)
                   (#_hsvHue (color widget))
                   (min (max (floor (* 255 (+ (/ (- y) radius) 0.5))) 0) 255)
                   (min (max (floor (* 255 (+ (/ x radius) 0.5))) 0) 255))
         (update-color-triangle widget)
         (#_update widget))))))

(defmethod color-widget-update ((widget color-hsv-widget) new-color)
  (#_setHsv (color widget)
            (#_hsvHue new-color)
            (#_saturation new-color)
            (#_value new-color))
  (update-color-triangle widget))
