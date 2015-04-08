#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(defpackage #:parasol-tools
  (:use #:parasol-dev #:parasol-document)
  (:nicknames #:org.shirakumo.parasol.tools)
  ;; meta.lisp
  (:export
   #:configurable-class
   #:configurable-slots
   #:configurable
   #:configurable-slot-changed
   #:define-superclass-method-wrapper

   #:descriptive-class
   #:object-title
   #:object-description)
  ;; tool.lisp
  (:export
   #:tool-class
   
   #:tool
   
   #:tool-title
   #:tool-description
   #:select
   #:deselect
   #:begin
   #:move
   #:end
   #:has-superclass
   #:define-tool
   #:find-tools))
