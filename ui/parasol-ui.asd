#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem parasol-ui
  :name "Parasol-Ui"
  :version "0.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :serial T
  :components ((:file "package")
               (:file "gl-compositing")
               (:file "framebuffer-target")
               (:file "document-view")
               (:file "layers")
               (:file "main-window"))
  :depends-on (:parasol
               :parasol-document
               :qtools-ui))
