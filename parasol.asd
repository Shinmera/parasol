#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem parasol
  :name "Parasol"
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A common lisp painting application."
  :homepage "https://Shinmera.github.io/parasol/"
  :bug-tracker "https://github.com/Shinmera/parasol/issues"
  :source-control (:git "https://github.com/Shinmera/parasol.git")
  :serial T
  :components ((:file "package")
               (:file "icons")
               (:file "load"))
  :depends-on (:verbose
               :qtools
               :qtcore
               :qtgui
               :qtopengl
               :qtools-ui))
