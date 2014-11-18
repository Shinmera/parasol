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
   #:current-layer)
  ;; history.lisp
  (:export
   #:history-item
   #:document
   #:undo
   #:redo

   #:history
   #:items
   #:record
   #:rewind
   #:size)
  ;; layer.lisp
  (:export
   #:meta-layer
   #:drawables
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
   #:draw-buffer
   #:rebuffer
   #:rebuffer-copy
   #:draw))
