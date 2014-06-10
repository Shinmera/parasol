#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defun main ()
  (ensure-smoke :qtopengl)
  (progn
    (make-qapplication)
    (with-objects ((window (make-instance 'main-window)))
      (#_show window)
      (#_exec *qapplication*)
      (finalize window))))
