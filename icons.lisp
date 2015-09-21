#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.dev)
(named-readtables:in-readtable :qtools)

(defvar *icon-path* (asdf:system-relative-pathname :parasol "assets/"))
(defvar *icons* (make-hash-table :test 'eql))

(defun make-icon (value)
  (etypecase value
    (null (q+:make-qicon))
    (qobject (q+:make-qicon value))
    (pathname (q+:make-qicon (uiop:native-namestring value)))
    (string (make-icon (asdf:system-relative-pathname :parasol value)))
    (list (q+:qicon-from-theme (first value) (make-icon (second value))))))

(defun icon (name)
  (gethash name *icons*))

(defun (setf icon) (value name)
  (setf (gethash name *icons*)
        (make-icon value)))

(defun cached-icon (name &optional icon-value)
  (or (icon name)
      (setf (icon name) icon-value)))
