#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defun %make-color-slider (color-widget)
  (make-instance 'ex-slider-widget :max 255 :on-change #'(lambda (val) (declare (ignore val)) (color-slider-widget-update color-widget))))

(defclass color-slider-widget ()
  ((%r :accessor r)
   (%g :accessor g)
   (%b :accessor b)
   (%parent :initarg :parent :initform (error "Parent required") :accessor parent))
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((widget color-slider-widget) &key)
  (new widget)
  (let ((layout (#_new QVBoxLayout))
        (r (%make-color-slider widget))
        (g (%make-color-slider widget))
        (b (%make-color-slider widget)))
    (#_setContentsMargins layout 0 0 0 0)
    (#_addWidget layout r)
    (#_addWidget layout g)
    (#_addWidget layout b)
    (#_setLayout widget layout)
    (setf (r widget) r
          (g widget) g
          (b widget) b)))

(defmethod color ((widget color-slider-widget))
  (#_new QColor (value (r widget)) (value (g widget)) (value (b widget))))

(defun color-slider-widget-update (widget)
  (color-widget-update (parent widget) (color widget)))

(defmethod color-widget-update ((widget color-slider-widget) new-color)
  (exs-update (r widget) (#_red new-color))
  (exs-update (g widget) (#_green new-color))
  (exs-update (b widget) (#_blue new-color)))

(defclass color-triangle-widget ()
  ((%color :initform (#_new QColor 0 0 0) :accessor color)
   (%pressed :initform NIL :accessor pressed)
   (%size :initform 0 :accessor size)
   (%pixmap :accessor pixmap)
   (%parent :initarg :parent :initform (error "Parent required") :accessor parent))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override ("paintEvent" paint-event)
             ("resizeEvent" resize-event)
             ("mousePressEvent" mouse-press-event)
             ("mouseReleaseEvent" mouse-release-event)
             ("mouseMoveEvent" mouse-move-event)))

(defmethod initialize-instance :after ((widget color-triangle-widget) &key)
  (new widget)
  (update-color-triangle widget))

(defmethod update-color-triangle ((widget color-triangle-widget))
  (let* ((color (color widget))
         (size (size widget))
         (radius (floor (/ size 2)))
         (wheel-width (floor (/ size 15)))
         (deg (- (/ (* (#_hsvHue color) PI) 180)))
         (pixmap (#_new QPixmap size size))
         (painter (#_new QPainter pixmap))
         (pen (#_new QPen))
         (hsv-gradient (#_new QConicalGradient (floor radius) (floor radius) 0))
         (ef1-gradient (#_new QLinearGradient
                              (floor (* radius 0.5)) (floor (* radius 0.5))
                              (floor (* radius 1.5)) (floor (* radius 0.5))))
         (ef2-gradient (#_new QLinearGradient
                              (floor (* radius 0.5)) (floor (* radius 0.5))
                              (floor (* radius 0.5)) (floor (* radius 1.5)))))
    (#_setRenderHint painter (#_QPainter::Antialiasing))
    (#_setRenderHint painter (#_QPainter::HighQualityAntialiasing))
    (#_fill pixmap (#_new QColor 0 0 0))
    
    ;; Draw hsv-gradient wheel
    (#_setColorAt hsv-gradient 0.0 (#_new QColor 255 0 0))
    (#_setColorAt hsv-gradient 0.166 (#_new QColor 255 255 0))
    (#_setColorAt hsv-gradient 0.333 (#_new QColor 0 255 0))
    (#_setColorAt hsv-gradient 0.5 (#_new QColor 0 255 255))
    (#_setColorAt hsv-gradient 0.666 (#_new QColor 0 0 255))
    (#_setColorAt hsv-gradient 0.833 (#_new QColor 255 0 255))
    (#_setColorAt hsv-gradient 1.0 (#_new QColor 255 0 0))
    (#_setBrush painter (#_new QBrush hsv-gradient))
    (#_drawEllipse painter 0 0 size size)
    ;; Clear inner
    (#_setBrush painter (#_new QBrush (#_new QColor 0 0 0)))
    (#_drawEllipse painter wheel-width wheel-width (- size (* wheel-width 2)) (- size (* wheel-width 2)))
    ;; Draw indicator
    (#_setWidthF pen 5.0)
    (#_setColor pen (#_new QColor 255 255 255))
    (#_setPen painter pen)
    (#_drawLine painter
                (round (+ (* (cos deg) (- radius wheel-width)) radius))
                (round (+ (* (sin deg) (- radius wheel-width)) radius))
                (round (+ (* (cos deg) radius) radius))
                (round (+ (* (sin deg) radius) radius)))
    ;; Draw square (@fixme triangle, positioning)
    (#_setColorAt ef1-gradient 0.0 (#_new QColor 0 0 0 255))
    (#_setColorAt ef1-gradient 1.0 (#_new QColor 0 0 0 0))
    (#_setColorAt ef2-gradient 0.0 (#_QColor::fromHsv (#_hsvHue color) 255 255 255))
    (#_setColorAt ef2-gradient 1.0 (#_new QColor 255 255 255 255))
    (#_fillRect painter (floor (* radius 0.5)) (floor (* radius 0.5)) radius radius (#_new QBrush ef2-gradient))
    (#_fillRect painter (floor (* radius 0.5)) (floor (* radius 0.5)) radius radius (#_new QBrush ef1-gradient))
    ;; Draw pick
    (#_setColor pen (#_new QColor 255 255 255))
    (#_setWidthF pen 2.0)
    (#_setPen painter pen)
    (#_setBrush painter (#_new QBrush))
    (#_drawEllipse painter (#_new QPoint
                                  (floor (* radius (+ 0.5 (/ (#_value color) 255))))
                                  (floor (* radius (+ 1.5 (- (/ (#_hsvSaturation color) 255)))))) 5 5)
    
    (#_end painter)
    (setf (pixmap widget) pixmap)))

(defmethod resize-event ((widget color-triangle-widget) event)
  (setf (size widget) (min (#_width widget) (#_height widget)))
  (update-color-triangle widget))

(defmethod paint-event ((widget color-triangle-widget) event)
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

(defmethod mouse-press-event ((widget color-triangle-widget) event)
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

(defmethod mouse-release-event ((widget color-triangle-widget) event)
  (setf (pressed widget) NIL)
  (color-widget-update (parent widget) (color widget)))

(defmethod mouse-move-event ((widget color-triangle-widget) event)
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

(defmethod color-widget-update ((widget color-triangle-widget) new-color)
  (setf (color widget) new-color)
  (update-color-triangle widget))

(defclass color-widget ()
  ((%slider-widget :accessor slider-widget)
   (%triangle-widget :accessor triangle-widget)
   (%current-color :accessor current-color))
  (:metaclass qt-class)
  (:qt-superclass "QWidget"))

(defmethod initialize-instance :after ((widget color-widget) &key)
  (new widget)
  (let ((slider (make-instance 'color-slider-widget :parent widget))
        (triangle (make-instance 'color-triangle-widget :parent widget))
        (current (#_new QLabel))
        (tabs (#_new QTabWidget))
        (layout (#_new QGridLayout)))
    (#_setAutoFillBackground current T)
    (#_setFixedHeight current 20)
    
    (#_addTab tabs slider "rgb")
    (#_addTab tabs triangle "hsv")
    
    (#_addWidget layout tabs 0 0)
    (#_addWidget layout current 1 0)
    (#_setLayout widget layout)

    (setf (slider-widget widget) slider
          (triangle-widget widget) triangle
          (current-color widget) current)
    (color-widget-update widget (color *current-brush*))))

(defmethod color-widget-update ((widget color-widget) new-color)
  (setf (color *current-brush*) new-color)
  (#_setColor (#_palette (current-color widget)) (#_QPalette::Background) new-color)
  (#_update (current-color widget))

  (color-widget-update (slider-widget widget) new-color)
  (color-widget-update (triangle-widget widget) new-color))
