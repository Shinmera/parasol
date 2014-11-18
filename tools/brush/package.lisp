#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(defpackage #:parasol-tools-brush
  (:use #:cl #:parasol #:parasol-document #:parasol-tools)
  (:nicknames #:org.shirakumo.parasol.tools.brush)
  ;; basic.lisp
  (:export
   #:linearly-sampled-brush
   #:distance
   #:draw-penpoint
   
   #:single-colored-brush
   #:color
   
   #:sized-brush
   #:size
   
   #:pressured-size-brush
   
   #:circle-tip-brush
   
   #:texture-brush
   #:texture
   
   #:basic-brush)
  ;; brush-class.lisp
  (:export
   #:brush-class
   #:brush-icon
   #:brush-label)
  ;; brush-tool.lisp
  (:export
   #:stroke
   #:add-point
   
   #:brush-tool
   #:current-brush)
  ;; brush.lisp
  (:export
   #:abstract-brush
   #:brush
   #:brush-options
   #:brush-label
   #:brush-icon
   
   #:draw-stroke
   #:define-brush))
