#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem parasol-tools-brush
  :name "Parasol-Tools-Brush"
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :serial T
  :components ((:file "package")
               (:file "brush-tool")
               (:file "brush")
               (:file "basic"))
  :depends-on (:parasol :parasol-document :parasol-tools))
