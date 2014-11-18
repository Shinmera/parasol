#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem parasol-ui
  :name "Parasol-Ui"
  :version "0.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :serial T
  :components ((:file "main-window")
               (:file "gizmo-bar")
               (:file "tab-area")
               (:file "document-view"))
  :depends-on (:parasol :parasol-document :parasol-tools))
