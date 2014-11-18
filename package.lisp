#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:parasol
  (:use #:cl #:qt #:qtools)
  (:nicknames #:org.shirakumo.parasol)
  (:export
   #:start))

(defun parasol::ensure-package (package)
  (etypecase package
    ((or symbol string) (find-package package))
    (package package)))

(defun parasol::re-export (package &optional (from '#:parasol))
  (setf from (parasol::ensure-package from))
  (do-external-symbols (symb (parasol::ensure-package package))
    (export symb from)))

(parasol::re-export '#:qtools)
(parasol::re-export '#:qt)
