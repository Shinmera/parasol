#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defvar *indentation-hints* (make-hash-table :test #'eq))

(defun define-indentation (symbol rule-form)
  (setf (gethash symbol *indentation-hints*)
        rule-form))

(defun initialize-slime ()
  (when (member "SWANK-INDENTATION" *modules* :test #'string=)
    (let* ((swank (find-package :swank))
           (tables (when swank (find-symbol (string '#:*application-hints-tables*) swank))))
      (when tables
        (set tables (cons *indentation-hints* (remove *indentation-hints* (symbol-value tables))))
        t))))

(define-indentation 'define-file-format '(1 &rest (&whole 2 &lambda 4 &body)))
(initialize-slime)
