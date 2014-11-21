#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.document)
(named-readtables:in-readtable :qtools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *target-backend* 'qimage-target))

(define-finalizable target (drawable)
  ((width :initarg :width :initform (error "WIDTH required.") :accessor width)
   (height :initarg :height :initform (error "HEIGHT required.") :accessor height)))

(defun make-target (width height)
  (make-instance *target-backend* :width width :height height))

(defgeneric to-image (target)
  (:method to-image ((target target))
    (error "TO-IMAGE of the target ~s is not implemented." target)))

(defgeneric get-painter (target)
  (:method get-painter ((target target))
    (error "GET-PAINTER of the target ~s is not implemented." target)))

(defmethod copy ((target target))
  (error "COPY of the target ~s is not implemented." target))

(defgeneric clear (target &optional color)
  (:method clear ((target target) &optional color)
    (declare (ignore color))
    (error "CLEAR of the target ~s is not implemented." target)))

;; QImage impl.
(define-finalizable qimage-target (target)
  ((image :initarg :image :accessor image :finalized T)))

(defmethod initialize-instance :after ((target qimage-target) &key)
  (unless (slot-boundp target 'image)
    (setf (image target)
          (make-image (width target) (height target)))))

(defmethod to-image ((target qimage-target))
  (image target))

(defmethod get-painter ((target qimage-target))
  (#_new QPainter (image target)))

(defmethod copy ((target qimage-target))
  (make-instance 'qimage-target :width (width target) :height (height target)
                                :image (copy (image target))))

(defmethod clear ((target qimage-target) &optional (color (#_Qt::transparent)))
  (#_fill (image target) color)
  target)

(defmethod draw ((target qimage-target) painter)
  (#_drawImage painter (#_new QPointF (x target) (y target)) (image target))
  target)

;; QGLFrameBufferObject
(define-finalizable gl-framebuffer-target (target)
  ((sampled-buffer :accessor sampled-buffer :finalized T)
   (buffer :accessor buffer :finalized T)))

(defmethod initialize-instance :after ((target gl-framebuffer-target) &key)
  (let ((format (#_new QGLFramebufferObjectFormat)))
    (#_setSamples format 4)
    (#_setAttachment format (#_QGLFramebufferObject::CombinedDepthStencil))
    (setf (sampled-buffer target)
          (#_new QGLFramebufferObject (width target) (height target) format)))
  (setf (buffer target)
        (#_new QGLFramebufferObject (width target) (height target))))

(defun blit-framebuffer-target (from to width height)
  (let ((rect (#_new QRect 0 0 width height)))
    (#_QGLFramebufferObject::blitFramebuffer from rect to rect)))

(defmethod to-image ((target gl-framebuffer-target))
  (#_toImage (buffer target)))

(defmethod get-painter ((target gl-framebuffer-target))
  (#_new QPainter (sampled-buffer target)))

(defmethod copy ((target gl-framebuffer-target))
  (let ((new (make-instance 'gl-framebuffer-target :width (width target) :height (height target))))
    (blit-framebuffer-target (sampled-buffer target) (sampled-buffer new) (width target) (height target))
    new))

(defmethod clear ((target gl-framebuffer-target) &optional (color (#_Qt::transparent)))
  (with-painter (painter (sampled-buffer target))
    (#_fillRect painter 0 0 (width target) (height target) color))
  target)

(defmethod draw ((target qimage-target) painter)
  ;; blit to non-sampled so we can access the texture.
  (blit-framebuffer-target (sampled-buffer target) (buffer target) (width target) (height target))

  ;; draw using OpenGL
  (#_beginNativePainting painter)

  (gl:enable :texture-2d)
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
  
