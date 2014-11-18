#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(defpackage #:parasol-tools
  (:use #:cl #:parasol-dev)
  (:nicknames #:org.shirakumo.parasol.tools)
  ;; default-options.lisp
  (:export
   #:widget-option
   #:integer-option
   #:double-option
   #:boolean-option
   #:string-option
   #:range-option
   #:color-option
   #:list-option
   #:add-item
   #:clear-items)
  ;; tool-class.lisp
  (:export
   #:tool-class
   #:tool-icon
   #:tool-label
   #:tool-description)
  ;; tool-option.lisp
  (:export
   #:tool-option
   #:label
   #:description
   #:tool
   #:on-change
   #:slot
   #:change
   #:define-tool-option)
  ;; tool.lisp
  (:export
   #:tool
   #:tool-options
   #:tool-label
   #:tool-description
   #:tool-icon
   #:tool-option
   #:select
   #:deselect
   #:begin
   #:move
   #:end
   #:define-tool))
