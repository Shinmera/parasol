#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem parasol
  :name "Parasol"
  :version "0.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A common lisp painting application."
  :homepage "https://github.com/Shinmera/parasol"
  :serial T
  :components ((:file "package")
               (:file "compat")
               (:file "icons")
               (:file "load"))
  :depends-on (:verbose :qtools))
