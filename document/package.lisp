#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(defpackage #:parasol-document
  (:use #:cl #:parasol-dev)
  (:nicknames #:org.shirakumo.parasol.document)
  ;; document.lisp
  (:export
   #:document
   #:current-layer
   #:add-layer)
  ;; history.lisp
  (:export
   #:history-item
   #:document
   #:applied
   #:undo
   #:redo

   #:history
   #:items
   #:age
   #:record
   #:rewind
   #:size
   #:current-history

   #:function-call-history-item
   #:slot-change-history-item
   #:vector-push-history-item)
  ;; image-op.lisp
  (:export
   #:make-painter
   #:with-painter)
  ;; layer.lisp
  (:export
   #:meta-layer
   #:drawables
   #:current-index
   #:current-drawable
   #:insert
   #:extract
   #:drawable-at
   #:activate
   #:size
   
   #:layer
   #:opacity
   #:mode
   #:visible
   #:draw
   #:draw-buffer

   #:adaptive-layer
   #:chunk-size
   #:ensure-fitting)
  ;; metadata.lisp
  (:export
   #:metadata
   #:fields
   #:field
   #:define-metadata-accessor
   #:matches)
  ;; pen.lisp
  (:export
   #:pointer-name
   #:device-name
   #:pen
   #:pointer
   #:device
   #:before
   #:x
   #:y
   #:z
   #:x-tilt
   #:y-tilt
   #:rotation
   #:pressure
   #:tangential-pressure
   #:real-time
   #:diff
   #:with-pen-values
   #:linear-interpolate)
  ;; primitives.lisp
  (:export
   #:positioned
   #:x
   #:y
   #:translate-to
   #:translate-away
   #:with-transformation
   #:with-translation-to
   #:with-translation-away
   
   #:drawable
   #:draw

   #:buffered
   #:buffer
   #:painter
   #:draw-buffer
   #:rebuffer
   #:rebuffer-copy
   #:draw

   #:adaptive-buffered
   #:chunk-size
   #:initial-size
   #:ensure-fitting)
  ;; target.lisp
  (:export
   #:*target-backend*
   #:target-backend
   #:target
   #:width
   #:height
   #:make-target
   #:make-target-instance
   #:make-target-from
   #:to-image
   #:painter
   #:copy
   #:clear
   #:draw
   #:fit
   
   #:qimage-target))
