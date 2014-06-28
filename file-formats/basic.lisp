#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)
(named-readtables:in-readtable :qt)

(define-file-format png
  (:save (document pathname)
    (with-objects ((image (render-region document)))
      (if (#_save image (uiop:native-namestring pathname) "png" 100)
          T
          (error "Unknown error saving image."))))
  
  (:load (document pathname)
    (let ((image (#_new QImage (uiop:native-namestring pathname) "png")))
      (if image
          (push-history-item (active-layer document) (make-instance 'raster-item :pixmap image))
          (error "Unknown error loading image.")))))

(define-file-format jpg
  (:save (document pathname)
    (with-objects ((image (render-region document)))
      (if (#_save image (uiop:native-namestring pathname) "jpg" 100)
          T
          (error "Unknown error saving image."))))
  
  (:load (document pathname)
    (let ((image (#_new QImage (uiop:native-namestring pathname) "jpg")))
      (if image
          (push-history-item (active-layer document) (make-instance 'raster-item :pixmap image))
          (error "Unknown error loading image.")))))

(define-file-format bmp
  (:save (document pathname)
    (with-objects ((image (render-region document)))
      (if (#_save image (uiop:native-namestring pathname) "bmp" 100)
          T
          (error "Unknown error saving image."))))
  
  (:load (document pathname)
    (let ((image (#_new QImage (uiop:native-namestring pathname) "bmp")))
      (if image
          (push-history-item (active-layer document) (make-instance 'raster-item :pixmap image))
          (error "Unknown error loading image.")))))