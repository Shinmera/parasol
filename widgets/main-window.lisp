#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(named-readtables:in-readtable :qtools)

(defvar *window*)

(with-widget-environment
  (define-widget main-window ("QMainWindow")
    ())

  (defmethod initialize-instance :after ((window main-window) &key)
    (unless (boundp '*window*)
      (error "Tried to create a main window without the proper context!"))
    (when *window*
      (error "A main window instance is already active!"))
    (setf *window* window)

    (#_setWindowTitle window (format NIL "Parasol v~a" (asdf:component-version (asdf:find-system :parasol))))
    (#_resize window 500 500)))

(defun main ()
  (let ((*window*))
    (with-main-window (window (make-instance 'main-window)))))
