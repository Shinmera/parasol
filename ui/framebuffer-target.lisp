#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

;; QGLFrameBufferObject
(defmethod make-target-instance ((backend (eql :framebuffer)) width height)
  (if *context*
      (make-instance 'framebuffer-target :width width :height height)
      (progn
        (v:warn :framebuffer-target "Falling back to qimage-target as no GL context is active.")
        (make-instance 'qimage-target :width width :height height))))

(define-finalizable framebuffer-target (target)
  ((sampled-buffer :initform NIL :accessor sampled-buffer :finalized T)
   (buffer :accessor buffer :finalized T)))

(defmethod (setf sampled-buffer) :around (value (target framebuffer-target))
  (unless (eql value (sampled-buffer target))
    (when (painter target)
      (finalize (painter target)))
    (when (sampled-buffer target)
      (finalize (sampled-buffer target)))
    (call-next-method)
    (setf (painter target)
          (make-painter value))))

(defvar *gl-format* NIL)

(defun ensure-gl-init ()
  (unless *gl-format*
    (let ((format (#_new QGLFormat)))
      (#_setAlpha format T)
      (#_setSampleBuffers format T)
      (setf *gl-format* format))))

(defun make-framebuffer (width height &optional (samples 0))
  (#_makeCurrent parasol-ui::*context*)
  (let ((format (#_new QGLFramebufferObjectFormat)))
    (when (< 0 samples)
      (#_setSamples format samples))
    (#_setAttachment format (#_QGLFramebufferObject::CombinedDepthStencil))
    (#_new QGLFramebufferObject width height format)))

(defmethod initialize-instance :after ((target framebuffer-target) &key)
  (ensure-gl-init)
  (#_makeCurrent parasol-ui::*context*)
  (setf (sampled-buffer target) (make-framebuffer (width target) (height target) 4))
  (setf (buffer target) (make-framebuffer (width target) (height target))))

(defun blit-framebuffer-target (from to width height &optional (x 0) (y 0))
  (setf width (round width)
        height (round height))
  (#_QGLFramebufferObject::blitFramebuffer
   to (#_new QRect (round x) (round y) width height)
   from (#_new QRect 0 0 width height))
  to)

(defmethod to-image ((target framebuffer-target))
  (#_toImage (buffer target)))

(defmethod copy ((target framebuffer-target))
  (let ((new (make-instance 'framebuffer-target :width (width target) :height (height target))))
    (blit-framebuffer-target (sampled-buffer target) (sampled-buffer new) (width target) (height target))
    new))

(defmethod clear ((target framebuffer-target) &optional (color (#_Qt::transparent)))
  (with-painter (painter (sampled-buffer target))
    (#_fillRect painter 0 0 (width target) (height target) color))
  target)

(defmethod draw ((target framebuffer-target) painter)
  ;; blit to non-sampled so we can access the texture.
  (blit-framebuffer-target (sampled-buffer target) (buffer target) (width target) (height target))
  
  ;; draw using OpenGL
  (#_beginNativePainting painter)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:blend-equation :func-add)
  
  (gl:enable :texture-2d)
  ;; Maybe ask stassats to incorporate support for this kinda thing...
  (gl:bind-texture :texture-2d (cffi:mem-ref (#_texture (buffer target)) :uint))
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

(defmethod fit ((target framebuffer-target) width height &key (x 0) (y 0))
  (unless (and (= (width target) width)
               (= (height target) height))
    (let ((new (make-framebuffer width height 4))
          (width (min width (width target)))
          (height (min height (height target))))
      (blit-framebuffer-target (sampled-buffer target) new width height x y)
      (setf (sampled-buffer target) new))
    (setf (buffer target) (make-framebuffer width height)))
  target)

(defmethod (setf target-backend) :around ((backend (eql :framebuffer)))
  (cond ((not (#_QGLFramebufferObject::hasOpenGLFramebufferObjects))
         (v:severe :framebuffer "Your system does not support OpenGL Frame Buffers!")
         (v:severe :framebuffer "Cannot enable framebuffer target. Drawing might be slow."))
        ((not (#_QGLFramebufferObject::hasOpenGLFramebufferBlit))
         (v:severe :framebuffer "Your system does not support OpenGL Frame Buffer Blitting!")
         (v:severe :framebuffer "Cannot enable framebuffer target. Drawing might be slow."))
        (T
         (call-next-method))))

(define-startup-hook set-framebuffer-target ()
  (setf (target-backend) :framebuffer))
