#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem parasol-tools-view
  :name "Parasol-Tools-View"
  :version "0.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :serial T
  :components ((:file "package")
               (:file "move-tool")
               (:file "rotate-tool")
               (:file "zoom-tool")
               (:file "restore-tool"))
  :depends-on (:parasol :parasol-document :parasol-tools :parasol-ui))
