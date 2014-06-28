#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defclass file-format ()
  ((%file-type :accessor file-type :allocation :class)
   (%filter :accessor filter :allocation :class)))

(defgeneric save-document (file-format document pathname)
  (:documentation ""))

(defgeneric load-document (file-format document pathname)
  (:documentation ""))

(defun find-file-formats ()
  (loop for class in (c2mop:class-direct-subclasses (find-class 'file-format))
        collect (class-name class)))

(defun file-formats-filter ()
  (mapcar #'(lambda (symbol) (filter (make-instance symbol)))
          (find-file-formats)))

(defmethod save-document ((file-format null) (document document) (pathname null))
  (let ((path (#_QFileDialog::getSaveFileName *window* "Choose File" (uiop:native-namestring (user-homedir-pathname))
                                              (format NIL "~{~a~^;;~}" (file-formats-filter)))))
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
          (when (save-document (make-instance symbol) document pathname)
            (setf (modified document) NIL
                  (name document) (pathname-name pathname))
            T)
          (error "Unknown file format: ~a" type)))))

(defmethod load-document ((file-format null) (document document) (pathname null))
  (let ((path (#_QFileDialog::getOpenFileName *window* "Choose File" (uiop:native-namestring (user-homedir-pathname))
                                              (format NIL "~{~a~^;;~}" (file-formats-filter)))))
    (when (< 0 (length path))
      (handler-case
          (progn (load-document nil document (uiop:parse-native-namestring path)) T)
        (error (err)
          (#_QMessageBox::critical *window* "Error" (format NIL "Error: ~a" err))
          NIL)))))

(defmethod load-document ((file-format null) (document document) pathname)
  (setf (file document) pathname)
  (let ((type (pathname-type pathname)))
    (let ((symbol (find-symbol (string-upcase type) "PARASOL")))
      (if (and symbol (find-class symbol))
          (load-document (make-instance symbol) document pathname)
          (error "Unknown file format: ~a" type)))))

(defmacro define-file-format (name &body options)
  (destructuring-bind (name &optional (type (string-downcase name)) (filter (format NIL "~a (*.~a)" name type))) (if (listp name) name (list name))
    `(progn
       (defclass ,name (file-format)
         ((%file-type :initform ,type :accessor file-type :allocation :class)
          (%filter :initform ,filter :accessor filter :allocation :class)))
       ,@(loop for option in options
               collect (ecase (first option)
                         ((:load :load-document)
                          `(defmethod load-document ((,(gensym "FILE-FORMAT") ,name) ,@(second option))
                             ,@(cddr option)))
                         ((:save :save-document)
                          `(defmethod save-document ((,(gensym "FILE-FORMAT") ,name) ,@(second option))
                             ,@(cddr option))))))))
