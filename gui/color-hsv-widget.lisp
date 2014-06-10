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
  (when (< 0 (size widget))
    (when (and (pixmap widget)
               (not (qt:null-qobject-p (pixmap widget))))
      (optimized-delete (pixmap widget)))
    (let* ((color (color widget))
           (size (size widget))
           (radius (floor (/ size 2)))
           (wheel-width (floor (/ size 15)))
           (deg (- (/ (* (#_hsvHue color) PI) 180))))
      (with-objects ((glformat (#_new QGLFormat))
                     (white (#_new QColor 255 255 255))
                     (black (#_new QColor 0 0 0)))
        (#_setAlpha glformat T)
        (#_setSampleBuffers glformat T)
        (with-objects ((pixmap (#_new QGLPixelBuffer size size glformat))
                       (painter (#_new QPainter pixmap)))
          (#_makeCurrent pixmap)
          (gl:enable :multisample)
          (gl:enable :line-smooth)
          (#_setRenderHint painter (#_QPainter::Antialiasing))
          (#_setRenderHint painter (#_QPainter::HighQualityAntialiasing))
          
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
          ;; Draw colour triangle
          (with-objects ((color (#_QColor::fromHsv (#_hsvHue color)
                                                   255 255)))
            (#_beginNativePainting painter)
            (gl:push-matrix)
            (gl:translate radius radius 0)
            (gl:rotate (- (#_hsvHue color)) 0 0 1)
            (let* ((radius (- radius wheel-width))
                   (side (* radius (cos (/ (* PI 30) 180)))))
              (gl:with-primitives :triangles
                (gl:color (/ (#_red color) 255)
                          (/ (#_green color) 255)
                          (/ (#_blue color) 255))
                (gl:vertex radius 0)
                (gl:color 1.0 1.0 1.0)
                (gl:vertex (- (/ radius 2)) side)
                (gl:color 0.0 0.0 0.0)
                (gl:vertex (- (/ radius 2)) (- side))))
            (gl:pop-matrix)
            (#_endNativePainting painter))        
          ;; Draw pick
          (with-objects ((pen (#_new QPen))
                         (brush (#_new QBrush)))
            (#_setColor pen white)
            (#_setWidthF pen 2.0)
            (#_setPen painter pen)
            (#_setBrush painter brush)
            (let* ((p (/ (* (- 150 (#_hsvHue color)) PI) 180))
                   (v (/ (#_value color) 255))
                   (s (- 1 (/ (#_saturation color) 255)))
                   ;; Triangle side length
                   (g (* 2 (- radius wheel-width) (cos (/ (* PI 30) 180))))
                   ;; Triangle height
                   (h (* g (/ (sqrt 3) 2)))
                   ;; Calculate Cartesian coordinates
                   (x (* v g (- s (/ 1 2))))
                   (y (* h (- (/ 2 3) v)))
                   ;; Turn into hue rotated coordinates
                   (x2 (- (* x (cos p))
                          (* y (sin p))))
                   (y2 (+ (* x (sin p))
                          (* y (cos p)))))
              (with-objects ((point (#_new QPoint
                                           (round (+ radius x2))
                                           (round (+ radius y2)))))
                (#_drawEllipse painter point 5 5))))
          
          (#_end painter)
          (setf (pixmap widget) (#_toImage pixmap)))))))

(defmethod resize-event ((widget color-hsv-widget) event)
  (setf (size widget) (min (#_width widget) (#_height widget)))
  (update-color-triangle widget))

(defmethod paint-event ((widget color-hsv-widget) event)
  (let ((painter (#_new QPainter widget)))
    (#_fillRect painter 0 0 (#_width widget) (#_height widget) (#_new QColor 0 0 0))
    (#_drawImage painter
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
        ((< r (- radius wheel-width))
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
         (let* ((p (/ (* (+ 210 (#_hsvHue (color widget))) PI) 180))
                ;; Triangle side length
                (g (* 2 (- radius wheel-width) (cos (/ (* PI 30) 180))))
                ;; Triangle height
                (h (* g (/ (sqrt 3) 2)))
                ;; Reverse rotation
                (x2 (- (* x (cos p))
                       (* y (sin p))))
                (y2 (+ (* x (sin p))
                       (* y (cos p))))
                ;; Calculate triangle coordinates
                (v (- (/ 2 3) (/ y2 h)))
                (s (+ (/ x2 (* v g)) (/ 1 2)))
                ;; Scale and cap
                (v (round (* (max (min v 1) 0) 255)))
                (s (round (* (- 1 (max (min s 1) 0)) 255))))
           (#_setHsv (color widget) (#_hsvHue (color widget)) s v))
         
         (update-color-triangle widget)
         (#_update widget))))))

(defmethod color-widget-update ((widget color-hsv-widget) new-color)
  (#_setHsv (color widget)
            (max (#_hsvHue new-color) 0)
            (#_saturation new-color)
            (#_value new-color))
  (update-color-triangle widget))

(defmethod finalize ((widget color-hsv-widget))
  (maybe-delete-qobject (pixmap widget))
  (maybe-delete-qobject (color widget))
  (setf (pixmap widget) NIL
        (color widget) NIL
        (parent widget) NIL))
