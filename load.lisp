#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.dev)

(defvar *startup-hooks* (make-hash-table :test 'eql))

(defmacro define-startup-hook (name () &body body)
  `(setf (gethash ',name *startup-hooks*)
         #'(lambda () ,@body)))

(defun remove-startup-hook (name)
  (remhash name *startup-hooks*))

(defun run-startup-hooks ()
  (maphash #'(lambda (k f)
               (v:info :parasol "Running startup hook ~a" k)
               (funcall f))
           *startup-hooks*))

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
  (ensure-loaded :parasol-ui)
  (ensure-loaded :parasol-tools-brush)
  (ensure-loaded :parasol-tools-view))

(defun start ()
  (qt:make-qapplication)
  (run-startup-hooks)
  (funcall (find-symbol "MAIN" "PARASOL-UI")))
