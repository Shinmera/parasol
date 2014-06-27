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
  ((%mode :initform NIL :accessor mode)
   (%tab-event :initform NIL :accessor tab-event)
   (%name :initarg :name :initform "Untitled" :accessor name)
   (%last-mouse :initform '(0 . 0) :accessor last-mouse)
   
   (%bg-brush :initform NIL :initarg :bg-brush :accessor bg-brush)
   (%offset-x :initform 0 :accessor offset-x)
   (%offset-y :initform 0 :accessor offset-y)
   (%active-layer-index :initform 0 :accessor active-layer-index)
   (%layers :initform (make-array 0 :adjustable T :fill-pointer 0) :accessor layers)
   (%buffer :initform () :accessor buffer)
   (%cutoff :initform NIL :accessor cutoff))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:override ("paintEvent" paint-event)
             ("tabletEvent" tablet-event)
             ("mousePressEvent" mouse-press-event)
             ("mouseReleaseEvent" mouse-release-event)
             ("mouseMoveEvent" mouse-move-event)
             ("resizeEvent" resize-event)))

(defmethod print-object ((document document) stream)
  (print-unreadable-object (document stream :type T :identity T)
    (format stream "~a" (name document)))
  document)

(defmethod initialize-instance :after ((document document) &key)
  (new document)
  (setf (background document) (merge-pathnames "background.png" *graphics*)
        (cutoff document) (make-instance 'cutoff :document document))
  (resize-canvas document (#_width document) (#_height document))
  (add-layer document))

(defmethod resize-canvas ((document document) width height)
  (when (buffer document) (maybe-delete-qobject (buffer document)))
  (setf (buffer document) (#_new QImage width height (#_QImage::Format_ARGB32_Premultiplied))))

(defmethod resize-event ((widget document) event)
  (resize-canvas widget (#_width widget) (#_height widget)))

(defmethod make-active ((widget document))
  )

;;; Stroke stuff
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
             (start-stroke widget
                           (pointer event)
                           (x event) (y event)
                           (x-tilt event) (y-tilt event)
                           (pressure event))))
         (progn
           (setf (mode widget) :mouse)
           (start-stroke widget
                         2 (#_x event) (#_y event)
                         0 0 *mouse-pressure*))))
    ((#_Qt::RightButton)
     (cycle-color *window*))
    ((#_Qt::MiddleButton)
     (setf (mode widget) :move)
     (setf (car (last-mouse widget)) (#_x event)
           (cdr (last-mouse widget)) (#_y event))))
  (#_update widget)
  (#_ignore event))

(defmethod mouse-move-event ((widget document) event)
  (case (mode widget)
    (:tablet
     (let ((event (tab-event widget)))
       (record-point widget
                     (x event) (y event)
                     (x-tilt event) (y-tilt event)
                     (pressure event)))
     (#_update widget))
    (:mouse
     (record-point widget
                   (#_x event) (#_y event)
                   0 0 *mouse-pressure*)
     (#_update widget))
    (:move
     (let ((last (last-mouse widget)))
       (move widget
             (- (#_x event) (car last))
             (- (#_y event) (cdr last)))
       (setf (car last) (#_x event)
             (cdr last) (#_y event)))
     (#_update widget)))
  (#_ignore event))

(defmethod mouse-release-event ((widget document) event)
  (case (mode widget)
    ((:tablet :mouse)
     (end-stroke widget)
     (#_update widget)))
  (setf (mode widget) NIL
        (tab-event widget) NIL)
  (#_ignore event))

(defmethod start-stroke ((document document) type x y x-tilt y-tilt pressure)
  (push-color *window*)
  (start-stroke (active-layer document)
                type
                (- x (offset-x document))
                (- y (offset-y document))
                x-tilt y-tilt pressure)
  document)

(defmethod record-point ((document document) x y x-tilt y-tilt pressure)
  (record-point (active-layer document)
                (- x (offset-x document))
                (- y (offset-y document))
                x-tilt y-tilt pressure)
  document)

(defmethod end-stroke ((document document))
  (end-stroke (active-layer document)))

;;; Painting stuff
(defmethod paint-event ((widget document) event)
  (with-objects ((painter (#_new QPainter widget)))
    (draw widget painter)
    (#_end painter)))

(defmethod draw ((document document) painter)
  (with-objects ((transform (#_new QTransform)))
    (#_translate  transform (offset-x document) (offset-y document))
    (#_setTransform (bg-brush document) transform)
    (#_fillRect painter (#_rect document) (bg-brush document)))
  (with-objects ((painter (#_new QPainter (buffer document)))
                 (transparent (#_new QColor 0 0 0 0)))
    (#_fill (buffer document) transparent)
    (#_translate painter (offset-x document) (offset-y document))
    (loop for layer across (layers document)
          do (draw layer painter))
    (#_end painter))
  (#_drawImage painter 0 0 (buffer document))
  (draw (cutoff document) painter))

;;; Layer stuff
(defmethod active-layer ((document document))
  (aref (layers document) (active-layer-index document)))

(defmethod (setf active-layer) (index (document document))
  (setf (active-layer-index document) index))

(defmethod add-layer ((document document) &key name)
  (let ((layer (make-instance 'layer :name (or name (format NIL "Layer ~d" (length (layers document)))))))
    (vector-push-extend layer (layers document))
    layer)
  (#_update document))

(defmethod remove-layer ((document document) &optional index)
  (when index
    (destroy (elt (layers document) index))
    (loop for i from index below (1- (length (layers document)))
          do (setf (aref (layers document) i)
                   (aref (layers document) (1+ i))))
    (vector-pop (layers document))
    (when (= (length (layers document)) 0)
      (add-layer document))
    (#_update document)))

(defmethod move-layer ((document document) index)
  (let* ((layers (layers document))
         (layer (active-layer document)))
    ;; Pop out
    (loop for i from (active-layer-index document) below (1- (length layers))
          do (setf (aref layers i)
                   (aref layers (1+ i))))
    ;; Shift
    (loop for i downfrom (1- (length layers)) above index
          do (setf (aref layers i)
                   (aref layers (1- i))))
    ;; Push in
    (setf (aref layers index) layer
          (active-layer-index document) index))
  (#_update document))

(defmethod move ((document document) x y)
  (incf (offset-x document) x)
  (incf (offset-y document) y))

;;; History stuff
(defmethod undo ((document document))
  (undo (active-layer document))
  (#_update document))

(defmethod redo ((document document))
  (redo (active-layer document))
  (#_update document))

;;; Background stuff
(defun remove-background (document)
  (let ((bg-brush (bg-brush document)))
    (when bg-brush
      (maybe-delete-qobject (#_textureImage bg-brush))
      (maybe-delete-qobject bg-brush))))

(defmethod (setf background) ((file pathname) (document document))
  (remove-background document)
  (setf (bg-brush document)
        (#_new QBrush (#_new QImage (uiop:native-namestring file)))))

(defmethod (setf background) ((rgb list) (document document))
  (remove-background document)
  (setf (bg-brush document)
        (#_new QBrush (#_new QColor (first rgb) (second rgb) (third rgb)))))

(defmethod (setf background) (brush-parameter (document document))
  (remove-background document)
  (setf (bg-brush document)
        (#_new QBrush brush-parameter)))

;;; Cutoff stuff
(defmethod find-real-size ((document document))
  (loop with (fx fy lx ly) = `(,most-positive-fixnum ,most-positive-fixnum
                                                     ,most-negative-fixnum ,most-negative-fixnum)
        for layer across (layers document)
        for (cfx cfy clx cly) = (find-real-size layer)
        do (when (< cfx fx) (setf fx cfx))
           (when (< cfy fy) (setf fy cfy))
           (when (> clx lx) (setf lx clx))
           (when (> cly ly) (setf ly cly))
        finally (return (list fx fy lx ly))))

(defmethod fit-cutoff ((document document))
  (destructuring-bind (fx fy lx ly) (find-real-size document)
    (let ((x fx) (y fy) (w (- lx fx)) (h (- ly fy))
          (c (cutoff document)))
      (setf (offset-x c) x
            (offset-y c) y
            (width c) w
            (height c) h))))

;;; Cleanup stuff
(defmethod finalize ((document document))
  (remove-background document)
  (loop for layer across (layers document)
        do (finalize layer))
  (setf (bg-brush document) NIL
        (layers document) NIL
        document NIL))

(defmethod destroy ((widget document))
  ;; add checks
  )
