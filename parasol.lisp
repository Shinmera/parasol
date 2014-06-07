#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defun main (&optional style)
  (with-main-window (window (make-instance 'main-window))
    (when style
      (#_QApplication::setStyle
       (#_QStyleFactory::create (ecase style
                                  (:cde "CDE")
                                  (:macintosh "Macintosh")
                                  (:windows "Windows")
                                  (:motif "Motif")))))))
