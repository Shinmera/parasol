#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass file-format ()
  ())

(defgeneric save-document (file-format document pathname)
  (:documentation ""))

(defgeneric load-document (file-format document pathname)
  (:documentation ""))

(defmethod save-document ((file-format null) (document document) (pathname null))
  (let ((path (#_QFileDialog::getSaveFileName *window* "Choose File" (uiop:native-namestring (user-homedir-pathname))))) ;; add proper filters
    (when (< 0 (length path))
      (handler-case
          (progn (save-document nil document (uiop:parse-native-namestring path)) T)
        (error (err)
          (#_QMessageBox::critical *window* "Error" (format NIL "Error: ~a" err))
          NIL)))))

(defmethod save-document ((file-format null) (document document) pathname)
  (setf (file document) pathname)
  (let ((type (pathname-type pathname)))
    (let ((symbol (find-symbol (string-upcase type) "PARASOL")))
      (if (and symbol (find-class symbol))
          (save-document (make-instance symbol) document pathname)
          (error "Unknown file format: ~a" type)))))
