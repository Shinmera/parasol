#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.document)
(named-readtables:in-readtable :qtools)

(defun make-image (width height &key (format (#_QImage::Format_ARGB32)) (fill (#_Qt::transparent)))
  (let ((image (#_new QImage width height format)))
    (#_fill image fill)
    image))

(defun fit-image (image width height &key (x 0) (y 0) (fill (#_Qt::transparent)))
  (cond ((and (= (#_width image) width)
              (= (#_height image) height))
         image)
        (T
         (let ((new (make-image width height :format (#_format image) :fill fill)))
           (with-finalizing ((painter (#_new QPainter new)))
             (#_drawImage painter x y image))
           new))))

(defun ensure-containable (x y image &key (chunk-size (#_width image)))
  (flet ((fit (n)
           (* chunk-size (ceiling (/ (+ (abs n) (/ chunk-size 2)) chunk-size)))))
    (let ((left (fit x))
          (top (fit y)))
      (cond
        ;; +-+-+
        ;; |3|4|
        ;; +-+-+
        ;; |2|1|
        ;; +-+-+
        ;; Yes this could be written much shorter,
        ;; but it gets gross to read, so I'll leave
        ;; it at this variant.
        ;; 1st Quadrant
        ((and (< 0 x) (< 0 y))
         (values
          (fit-image image
                     (max (#_width image) left)
                     (max (#_height image) top))
          0 0))
        ;; 2nd Quadrant
        ((and (<= x 0) (< 0 y))
         (values
          (fit-image image
                     (+ left (#_width image))
                     (max (#_height image) top)
                     :x left)
          (- left) 0))
        ;; 3rd Quadrant
        ((and (<= x 0) (<= y 0))
         (values
          (fit-image image
                     (+ left (#_width image))
                     (+ top (#_height image))
                     :x left :y top)
          (- left) (- top)))
        ;; 4th Quadrant
        ((and (< 0 x) (<= y 0))
         (values
          (fit-image image
                     (max (#_width image) left)
                     (+ top (#_height image))
                     :y top)
          0 (- top)))
        (T (error "HWAT"))))))
