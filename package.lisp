#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:parasol-dev
  (:use #:cl #:qt #:qtools)
  (:nicknames #:org.shirakumo.parasol.dev)
  (:export
   #:start
   #:define-startup-hook
   #:remove-startup-hook

   #:icon
   #:make-icon
   #:cached-icon))

(defun parasol-dev::ensure-package (package)
  (etypecase package
    ((or symbol string) (find-package package))
    (package package)))

(defun parasol-dev::re-export (package &optional (from '#:parasol-dev))
  (setf from (parasol-dev::ensure-package from))
  (do-external-symbols (symb (parasol-dev::ensure-package package))
    (export symb from)))

(parasol-dev::re-export '#:qtools)
(parasol-dev::re-export '#:qt)

(defpackage #:parasol
  (:use #:cl #:parasol-dev)
  (:nicknames #:org.shirakumo.parasol)
  (:export
   #:start))
