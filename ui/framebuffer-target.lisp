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
    (let ((format (q+:make-qglformat)))
      (setf (q+:alpha format) T)
      (setf (q+:sample-buffers format) T)
      (setf *gl-format* format))))

(defun make-framebuffer (width height &optional (samples 0))
  (q+:make-current parasol-ui::*context*)
  (let ((format (q+:make-qglframebufferobjectformat)))
    (when (< 0 samples)
      (setf (q+:samples format) samples))
    (setf (q+:attachment format) (q+:qglframebufferobject.combined-depth-stencil))
    (let ((buffer (q+:make-qglframebufferobject width height format)))
      (finalize format)
      (with-finalizing ((painter (q+:make-qpainter buffer)))
        (q+:begin-native-painting painter)
        (gl:clear-color 0 0 0 0)
        (gl:clear :color-buffer-bit)
        (q+:end-native-painting painter))
      buffer)))

(defmethod initialize-instance :after ((target framebuffer-target) &key)
  (ensure-gl-init)
  (q+:make-current parasol-ui::*context*)
  (setf (sampled-buffer target) (make-framebuffer (width target) (height target) 4))
  (setf (buffer target) (make-framebuffer (round (width target)) (round (height target)))))

(defun blit-framebuffer-target (from to width height &optional (x 0) (y 0))
  (setf width (round width)
        height (round height))
  (q+:qglframebufferobject-blit-framebuffer
   to (q+:make-qrect (round x) (round y) width height)
   from (q+:make-qrect 0 0 width height))
  to)

(defmethod to-image ((target framebuffer-target))
  (q+:to-image (buffer target)))

(defmethod copy ((target framebuffer-target))
  (let ((new (make-instance 'framebuffer-target :width (width target) :height (height target))))
    (blit-framebuffer-target (sampled-buffer target) (sampled-buffer new) (width target) (height target))
    new))

(defmethod clear ((target framebuffer-target) &optional (color (q+:qt.transparent)))
  (with-painter (painter (sampled-buffer target))
    (q+:fill-rect painter 0 0 (width target) (height target) color))
  target)

(defmethod draw ((target framebuffer-target) painter)
  ;; blit to non-sampled so we can access the texture.
  (blit-framebuffer-target (sampled-buffer target) (buffer target) (width target) (height target))
  ;; draw using OpenGL
  (q+:begin-native-painting painter)
  (gl:enable :blend)
  (gl-set-blending (enum-value (q+:composition-mode painter)))  
  
  (gl:enable :texture-2d)
  ;; Maybe ask stassats to incorporate support for this kinda thing...
  (gl:bind-texture :texture-2d (q+:texture (buffer target)))
  (gl:with-primitives :quads
    (gl:tex-coord 0 1)
    (gl:vertex 0 0)
    (gl:tex-coord 1 1)
    (gl:vertex (1+ (width target)) 0)
    (gl:tex-coord 1 0)
    (gl:vertex (1+ (width target)) (1+ (height target)))
    (gl:tex-coord 0 0)
    (gl:vertex 0 (1+ (height target))))
  
  (q+:end-native-painting painter))

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
  (cond ((not (q+:qglframebufferobject-has-open-glframebuffer-objects))
         (v:severe :framebuffer "Your system does not support OpenGL Frame Buffers!")
         (v:severe :framebuffer "Cannot enable framebuffer target. Drawing might be slow."))
        ((not (q+:qglframebufferobject-has-open-glframebuffer-blit))
         (v:severe :framebuffer "Your system does not support OpenGL Frame Buffer Blitting!")
         (v:severe :framebuffer "Cannot enable framebuffer target. Drawing might be slow."))
        (T
         (call-next-method))))

(define-hook (:startup framebuffer-backend) ()
  (setf (target-backend) :framebuffer))
