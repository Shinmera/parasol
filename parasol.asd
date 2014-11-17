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
               (:file "array")
               (:module "document"
                :components ((:file "image-op")
                             (:file "pen")
                             (:file "metadata")
                             (:file "primitives")
                             (:file "history")
                             (:file "layer")
                             (:file "document")))
               (:module "tools"
                :components ((:file "tool-option")
                             (:file "default-options")
                             (:file "tool-class")
                             (:file "tool")
                             (:file "brush-tool")
                             (:module "brushes"
                              :components ((:file "brush-class")
                                           (:file "brush")
                                           (:file "basic")))))
               (:module "widgets"
                :components ((:file "main-window")
                             (:file "gizmo-bar")
                             (:file "tab-area")
                             (:file "document-view"))))
  :depends-on (:verbose :qtools))
