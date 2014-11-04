#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
;; This file is used for various system and compatibility
;; checks to make sure parasol has what it needs to run.

(defvar *qapplication* NIL)
(defun test-compatibility ()
  #-:ccl (warn "Parasol is only supported on CCL. Proceed at your own risk.")

  ;; We have to init here.
  (setf *qapplication* (make-qapplication))

  ;; QThread needs to be compiled in manually.
  ;; We might want to provide functions to do that...
  (assert (find-qclass "QThread") () "SmokeQt needs to be compiled with QThread class support.")

  ;; We need OpenGL support for better drawing.
  (ensure-smoke :qtopengl))

(test-compatibility)
