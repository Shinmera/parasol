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
               (:file "generics")
               (:module "curves"
                :components ((:file "curve")
                             (:file "spline")
                             (:file "linear")))
               (:module "brushes"
                :components ((:file "brush")
                             (:file "texture-brush")))
               (:module "document"
                :components ((:file "stroke")
                             (:file "layer")
                             (:file "document")
                             (:file "canvas")))
               (:module "gui"
                :components ((:file "ex-slider-widget")
                             (:file "file-chooser-widget")
                             (:file "brush-ui")
                             (:file "brush-widget")
                             (:file "color-rgb-widget")
                             (:file "color-hsv-widget")
                             (:file "color-widget")
                             (:file "curve-dialog")
                             (:file "layer-widget")
                             (:file "documents-widget")
                             (:file "repl-widget")
                             (:file "main-window")))
               (:file "parasol"))
  :depends-on (:qt
               :cl-opengl
               :uiop
               :closer-mop))
