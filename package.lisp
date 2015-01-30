#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:parasol-dev
  (:use #:cl+qt)
  (:nicknames #:org.shirakumo.parasol.dev)
  (:export
   #:start
   #:define-startup-hook
   #:remove-startup-hook

   #:icon
   #:make-icon
   #:cached-icon))

(do-symbols (symbol '#:org.shirakumo.parasol.dev)
  (export symbol '#:org.shirakumo.parasol.dev))

(defpackage #:parasol
  (:use #:parasol-dev)
  (:nicknames #:org.shirakumo.parasol)
  (:export
   #:start))
