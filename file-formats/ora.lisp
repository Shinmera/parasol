#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(defparameter *ora-compositing-mode-list*
  '(("svg:src-over" 0)
    ("svg:dst-out" 8)
    ("svg:src-atop" 9) ;; not in the standard
    ("svg:multiply" 13)
    ("svg:screen" 14)
    ("svg:overlay" 15)
    ("svg:darken" 16)
    ("svg:lighten" 17)
    ("svg:color-dodge" 18)
    ("svg:color-burn" 19)
    ("svg:hard-light" 20)
    ("svg:soft-light" 21)
    ("svg:difference" 22)
    ("svg:exclusion" 23) ;; not in the standard
    ))

(defparameter *ora-num->mode*
  (let ((map (make-hash-table :test 'eql)))
    (loop for (val key) in *ora-compositing-mode-list*
          do (setf (gethash key map) val))
    map))

(defparameter *ora-mode->num*
  (let ((map (make-hash-table :test 'equalp)))
    (loop for (key val) in *ora-compositing-mode-list*
          do (setf (gethash key map) val))
    map))

(defgeneric ora-composite (val)
  (:method ((val number))
    (or (gethash val *ora-num->mode*)
        (progn (warn "Transforming unknown mode ~a to svg:src-over." val)
               "svg:src-over")))
  (:method ((val string))
    (or (gethash val *ora-mode->num*)
        (progn (warn "Transforming unknown mode ~a to Normal (0)." val)
               0))))

(define-file-format (ora "ora" "Open Raster (*.ora)")
  (:load (document pathname)
    ;; Remove default layer
    (finalize (vector-pop (layers document)))
    (with-temporary-directory (dir)
      (handler-bind ((error #'(lambda (err) (invoke-debugger err))))
        (zip:unzip pathname dir :verbose T)
        (with-open-file (s (merge-pathnames "stack.xml" dir) :direction :input :if-does-not-exist :error)
          (loop with layers = (lquery:$ (initialize s) "layer")
                for i downfrom (1- (length layers)) to 0
                for layer = (aref layers i)
                do (v:debug :ora "Loading layer ~a" (lquery:$ layer (serialize) (node)))
                   (let* ((name (lquery:$ layer (attr :name) (node)))
                          (x (parse-integer (lquery:$ layer (attr :x) (node))))
                          (y (parse-integer (lquery:$ layer (attr :y) (node))))
                          (opacity (parse-float (lquery:$ layer (attr :opacity) (node))))
                          (mode (ora-composite (lquery:$ layer (attr :composite-op) (node))))
                          (src (merge-pathnames (lquery:$ layer (attr :src) (node)) dir))
                          (instance (make-instance 'layer
                                                   :name (or name (pathname-name src) (format NIL "Layer ~d" i))
                                                   :offset-x x :offset-y y :opacity opacity :mode mode))
                          (image (#_new QImage (uiop:native-namestring src))))
                     (with-cleanup-on-error (image) (error)
                       (when (#_isNull image)
                         (error "Failure loading layer: ~a" (plump:serialize layer)))
                       (push-history-item (make-instance 'raster-item :pixmap image :offset-x x :offset-y y) instance)
                       (vector-push-extend instance (layers document)))))
          T))))
  
  (:save (document pathname)
    (with-temporary-directory (dir)
      (let ((data (merge-pathnames "data/" dir)))
        (ensure-directories-exist data)
        (with-open-file (s (merge-pathnames "stack.xml" dir) :direction :output)
          (format s "<?xml version='1.0' encoding='UTF-8'?>~%~
                       <image version=\"0.0.4\" w=\"~a\" h=\"~a\">~%~
                         <stack>~%" (width (cutoff document)) (height (cutoff document)))
          (loop for layer across (layers document)
                for i from 1
                do (format s "<layer name=\"~a\" x=\"~a\" y=\"~a\" opacity=\"~a\" composite-op=\"~a\" src=\"data/~5,'0d.png\" />~%"
                           (name layer) (offset-x layer) (offset-y layer)
                           (opacity layer) (ora-composite (mode layer)) i)
                   (unless (#_save (pixmap layer) (uiop:native-namestring (merge-pathnames (format NIL "~5,'0d.png" i) data))
                                   "png" 100)
                     (error "Failure saving layer: ~a" layer)))
          (format s "</stack>~%</image>"))
        (with-open-file (s (merge-pathnames "mimetype" dir) :direction :output)
          (format s "image/openraster"))
        (zip:zip pathname dir :if-exists :supersede)))))
