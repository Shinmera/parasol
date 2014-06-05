#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.tymoonnext.parasol.asdf
  (:use #:cl #:asdf))
(in-package #:org.tymoonnext.parasol.asdf)

(defsystem parasol
  :name "Parasol Painting Program"
  :version "0.1.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A painting application with focus on tablets."
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:module "curves"
                :components ((:file "curve")
                             (:file "spline")))
               (:file "brush")
               (:file "stroke")
               (:file "canvas")
               (:module "gui"
                :components ((:file "canvas-widget")
                             (:file "brush-widget")
                             (:file "color-widget")
                             (:file "layer-widget")
                             (:file "main-window")))
               (:file "parasol"))
  :depends-on (:qt :uiop))
