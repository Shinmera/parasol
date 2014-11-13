#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)

;; Metaclass to automatically define
;; tool options and integrate into
;; the UI, similar to how I did done
;; dune it with the brushes?
(defclass tool ()
  ())

(defgeneric select (tool))

(defgeneric deselect (tool))

(defgeneric begin (tool pen))

(defgeneric move (tool pen))

(defgeneric end (tool pen))
