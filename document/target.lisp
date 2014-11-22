#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.document)
(named-readtables:in-readtable :qtools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *target-backend* 'qimage-target))

(define-finalizable target ()
  ((width :initarg :width :initform (error "WIDTH required.") :accessor width)
   (height :initarg :height :initform (error "HEIGHT required.") :accessor height)
   (painter :initform NIL :accessor painter)))

(defmethod print-object ((target target) stream)
  (print-unreadable-object (target stream :type T :identity T)
    (format stream "~a/~a ~ax~a" (x target) (y target) (width target) (height target)))
  target)

(defun make-target (width height)
  (make-instance *target-backend* :width width :height height))

(defun make-target-from (pathname)
  (let* ((image (#_new QImage (uiop:native-namestring pathname)))
         (target (make-target (#_width image) (#_height image))))
    (with-painter (painter target)
      (#_drawImage painter 0 0 image))
    (finalize image)
    target))

(defgeneric to-image (target)
  (:method ((target target))
    (error "TO-IMAGE of the target ~s is not implemented." target)))

(defgeneric painter (target)
  (:method ((widget widget))
    (#_new QPainter widget))
  (:method ((object qobject))
    (#_new QPainter object)))

(defmethod copy ((target target))
  (error "COPY of the target ~s is not implemented." target))

(defgeneric clear (target &optional color)
  (:method ((target target) &optional color)
    (declare (ignore color))
    (error "CLEAR of the target ~s is not implemented." target)))

(defmethod draw ((target target) painter)
  (error "DRAW of the target ~s is not implemented." target))

(defgeneric fit (target width height &key x y)
  (:method ((target target) width height &key x y)
    (declare (ignore width height x y))
    (error "FIT of the target ~s is not implemented." target))
  (:method :after ((target target) width height &key x y)
    (declare (ignore x y))
    (setf (width target) width
          (height target) height)))

;; QImage impl.
(define-finalizable qimage-target (target)
  ((image :initarg :image :initform NIL :accessor image :finalized T)))

(defmethod (setf image) :around (value (target qimage-target))
  (unless (eql value (image target))
    (when (painter target)
      (finalize (painter target)))
    (when (image target)
      (finalize (image target)))
    (call-next-method)
    (setf (painter target)
          (make-painter value))))

(defmethod initialize-instance :after ((target qimage-target) &key)
  (unless (image target)
    (let ((image (#_new QImage (width target) (height target) (#_QImage::Format_ARGB32))))
      (#_fill image (#_Qt::transparent))
      (setf (image target) image))))

(defmethod to-image ((target qimage-target))
  (image target))

(defmethod copy ((target qimage-target))
  (make-instance 'qimage-target :width (width target) :height (height target)
                                :image (copy (image target))))

(defmethod clear ((target qimage-target) &optional (color (#_Qt::transparent)))
  (#_fill (image target) color)
  target)

(defmethod draw ((target qimage-target) painter)
  (#_drawImage painter 0 0 (image target))
  target)

(defmethod fit ((target qimage-target) width height &key (x 0) (y 0))
  (unless (and (= (width target) width)
               (= (height target) height))
    (let ((new (#_new QImage width height (#_QImage::Format_ARGB32))))
      (#_fill new (#_Qt::transparent))
      (with-finalizing ((painter (#_new QPainter new)))
        (#_drawImage painter x y (image target)))
      (setf (image target) new)))
  target)

;; QGLFrameBufferObject
(define-finalizable gl-framebuffer-target (target)
  ((sampled-buffer :initform NIL :accessor sampled-buffer :finalized T)
   (buffer :accessor buffer :finalized T)))

(defmethod (setf sampled-buffer) :around (value (target gl-framebuffer-target))
  (unless (eql value (sampled-buffer target))
    (when (painter target)
      (finalize (painter target)))
    (when (sampled-buffer target)
      (finalize (sampled-buffer target)))
    (call-next-method)
    (setf (painter target)
          (make-painter value))))

(defvar *gl-format*
  (let ((format (#_new QGLFormat)))
    (#_setAlpha format T)
    (#_setSampleBuffers format T)
    format))

(defvar *gl-context*
  (let ((context (#_new QGLContext *gl-format*)))
    (#_create context)
    context))

(defun make-sampled-framebuffer (width height &optional (samples 4))
  (#_makeCurrent *gl-context*)
  (let ((format (#_new QGLFramebufferObjectFormat)))
    (#_setSamples format samples)
    (#_setAttachment format (#_QGLFramebufferObject::CombinedDepthStencil))
    (#_new QGLFramebufferObject width height format)))

(defmethod initialize-instance :after ((target gl-framebuffer-target) &key)
  (#_makeCurrent *gl-context*)
  (setf (sampled-buffer target) (make-sampled-framebuffer (width target) (height target)))
  (setf (buffer target) (#_new QGLFramebufferObject (width target) (height target))))

(defun blit-framebuffer-target (from to width height)
  (let ((rect (#_new QRect 0 0 width height)))
    (#_QGLFramebufferObject::blitFramebuffer from rect to rect)))

(defmethod to-image ((target gl-framebuffer-target))
  (#_toImage (buffer target)))

(defmethod copy ((target gl-framebuffer-target))
  (let ((new (make-instance 'gl-framebuffer-target :width (width target) :height (height target))))
    (blit-framebuffer-target (sampled-buffer target) (sampled-buffer new) (width target) (height target))
    new))

(defmethod clear ((target gl-framebuffer-target) &optional (color (#_Qt::transparent)))
  (with-painter (painter (sampled-buffer target))
    (#_fillRect painter 0 0 (width target) (height target) color))
  target)

(defmethod draw ((target gl-framebuffer-target) painter)
  ;; draw using OpenGL
  (#_beginNativePainting painter)

  ;; blit to non-sampled so we can access the texture.
  (blit-framebuffer-target (sampled-buffer target) (buffer target) (width target) (height target))

  (gl:enable :texture-2d)
  (gl:enable :multisample)
  (gl:enable :cull-face)
  (gl:tex-env :texture-env :texture-env-mode :replace)
  (gl:bind-texture :texture-2d (#_texture (buffer target)))
  (gl:with-primitives :quads
    (gl:tex-coord 0 1)
    (gl:vertex 0 0)
    (gl:tex-coord 1 1)
    (gl:vertex (1+ (width target)) 0)
    (gl:tex-coord 1 0)
    (gl:vertex (1+ (width target)) (1+ (height target)))
    (gl:tex-coord 0 0)
    (gl:vertex 0 (1+ (height target))))
  
  (#_endNativePainting painter))
  
(defmethod fit ((target gl-framebuffer-target) width height &key (x 0) (y 0))
  (unless (and (= (width target) width)
               (= (height target) height))
    (let ((new (make-sampled-framebuffer width height))
          (width (min width (width target)))
          (height (min height (height target))))
      (#_QGLFramebufferObject::blitFramebuffer
       (sampled-buffer target) (#_new QRect 0 0 width height) 
       new (#_new QRect x y width height))
      (setf (sampled-buffer target) new))
    (setf (buffer target) (#_new QGLFramebufferObject width height)))
  target)
