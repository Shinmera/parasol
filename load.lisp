#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.dev)

(defun load-system (system)
  #+:quicklisp (ql:quickload system)
  #-:quicklisp (asdf:load-system system))

(defun ensure-loaded (system)
  (unless (asdf:component-loaded-p system)
    (load-system system)))

(defmethod asdf:perform :after ((op asdf:load-op) (system (eql (asdf:find-system :parasol))))
  ;; !STUB
  ;; some kind of system to automate this
  ;; or at least make it hookable
  (ensure-loaded :parasol-ui))

(defun start ()
  (qt:make-qapplication)
  (trigger :startup)
  (funcall (find-symbol "MAIN" "PARASOL-UI")))
