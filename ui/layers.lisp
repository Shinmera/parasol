#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(define-widget layers (QWidget qui:panel)
  ()
  (:default-initargs :title "Layers"))

(define-subwidget (layers container) (q+:make-qwidget)
  (setf (qui:widget :center layers) container))

(define-subwidget (layers list) (make-instance 'qui:listing)
  (setf (q+:minimum-height list) 200))

(define-subwidget (layers layout) (q+:make-qgridlayout container)
  (q+:add-widget layout list 0 0 1 1)
  (flet ((mkbutton (name row column row-span column-span)
           (let ((button (q+:make-qpushbutton name layers)))
             (setf (q+:object-name button) name)
             (q+:add-widget layout button row column row-span column-span))))
    (mkbutton "+" 1 0 1 1)
    (mkbutton "-" 1 1 1 1)
    (mkbutton "v" 1 2 1 1)
    (mkbutton "^" 1 3 1 1)
    (mkbutton "m" 1 4 1 1)
    (mkbutton "c" 1 5 1 1)))

(define-slot (layers button) ()
  (declare (connected (find-children layers "QPushButton") (clicked)))
  (let ((name (q+:object-name (q+:sender layers))))
    (when (< 0 (length name))
      (case (char name 0)
        (#\+ (add-layer (current-document)))
        (#\- (remove-layer (current-document)))
        (#\v (move-layer (current-document) -1))
        (#\^ (move-layer (current-document) 1))
        (#\m (merge-layer (current-document)))
        (#\c (clone-layer (current-document)))))))
