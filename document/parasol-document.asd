#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem parasol
  :name "Parasol-Document"
  :version "0.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :serial T
  :components ((:file "array")
               (:file "image-op")
               (:file "pen")
               (:file "metadata")
               (:file "primitives")
               (:file "history")
               (:file "layer")
               (:file "document"))
  :depends-on (:parasol))
