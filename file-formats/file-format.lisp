#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass file-format ()
  ())

(defgeneric save-document (file-format document pathname)
  (:documentation ""))

(defgeneric load-document (file-format document pathname)
  (:documentation ""))
