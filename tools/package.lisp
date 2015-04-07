#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(defpackage #:parasol-tools
  (:use #:parasol-dev #:parasol-document)
  (:nicknames #:org.shirakumo.parasol.tools)
  ;; tool-class.lisp
  (:export
   #:tool-class
   #:tool-title
   #:tool-display
   #:tool-description
   #:check-display-slot)
  ;; tool.lisp
  (:export
   #:tool
   #:tool-options
   #:tool-title
   #:tool-description
   #:tool-display
   #:select
   #:deselect
   #:begin
   #:move
   #:end
   #:define-tool
   #:find-tools))
