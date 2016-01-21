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
   #:start)
  (:export
   #:hook
   #:hooks
   #:add-hook
   #:remove-hook
   #:define-hook
   #:trigger))

(do-symbols (symbol '#:org.shirakumo.parasol.dev)
  (export (list symbol) '#:org.shirakumo.parasol.dev))

(defpackage #:parasol
  (:use #:parasol-dev)
  (:nicknames #:org.shirakumo.parasol)
  (:export
   #:start))
