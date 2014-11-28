#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(defun gl-set-blending (mode)
  #+:nvidia-blending
  (nv-compositing-mode mode)
  #-:nvidia-blending
  (compositing-mode mode))

(defun compositing-mode (mode)
  (ecase mode
    (0 ;(#_QPainter::CompositionMode_SourceOver)
     (gl:blend-func :one :one-minus-src-alpha))
    (1 ;(#_QPainter::CompositionMode_DestinationOver)
     (gl:blend-func :one-minus-dst-alpha :one))
    (2 ;(#_QPainter::CompositionMode_Clear)
     (gl:blend-func :zero :zero))
    (3 ;(#_QPainter::CompositionMode_Source)
     (gl:blend-func :one :zero))
    (4 ;(#_QPainter::CompositionMode_Destination)
     (gl:blend-func :zero :one))
    (5 ;(#_QPainter::CompositionMode_SourceIn)
     (gl:blend-func :dst-alpha :zero))
    (6 ;(#_QPainter::CompositionMode_DestinationIn)
     (gl:blend-func :zero :src-alpha))
    (7 ;(#_QPainter::CompositionMode_SourceOut)
     (gl:blend-func :one-minus-dst-alpha :zero))
    (8 ;(#_QPainter::CompositionMode_DestinationOut)
     (gl:blend-func :zero :one-minus-src-alpha))
    (9 ;(#_QPainter::CompositionMode_SourceAtop)
     (gl:blend-func :dst-alpha :one-minus-src-alpha))
    (10 ;(#_QPainter::CompositionMode_DestinationAtop)
     (gl:blend-func :one-minus-dst-alpha :src-alpha))
    (11 ;(#_QPainter::CompositionMode_Xor)
     (gl:blend-func :one-minus-dst-alpha :one-minus-src-alpha))
    (12 ;(#_QPainter::CompositionMode_Plus)
     (gl:blend-func :one :one))
    (13 ;(#_QPainter::CompositionMode_Multiply)
     (gl:blend-func :dst-color :one-minus-src-alpha))
    (14 ;(#_QPainter::CompositionMode_Screen)
     (gl:blend-func :one :one-minus-src-color))
    ;; (15 ;(#_QPainter::CompositionMode_Overlay)
    ;;  (gl:blend-func ))
    (16 ;(#_QPainter::CompositionMode_Darken)
     (gl:blend-func :one :one)
     (gl:blend-equation :func-min))
    (17 ;(#_QPainter::CompositionMode_Lighten)
     (gl:blend-func :one :one)
     (gl:blend-equation :func-max))
    ;; (18 ;(#_QPainter::CompositionMode_ColorDodge)
    ;;  (gl:blend-func ))
    ;; (19 ;(#_QPainter::CompositionMode_ColorBurn)
    ;;  (gl:blend-func ))
    ;; (20 ;(#_QPainter::CompositionMode_HardLight)
    ;;  (gl:blend-func ))
    ;; (21 ;(#_QPainter::CompositionMode_SoftLight)
    ;;  (gl:blend-func ))
    ;; (22 ;(#_QPainter::CompositionMode_Difference)
    ;;  (gl:blend-func ))
    (23 ;(#_QPainter::CompositionMode_Exclusion)
     (gl:blend-func :one :one)
     (gl:blend-equation :func-subtract))))

(defun nv-compositing-mode (mode)
  (ecase mode
    (0 ;(#_QPainter::CompositionMode_SourceOver)
     (gl:blend-equation :src-over-nv))
    (1 ;(#_QPainter::CompositionMode_DestinationOver)
     (gl:blend-equation :dst-over-nv))
    (2 ;(#_QPainter::CompositionMode_Clear)
     (gl:blend-equation :zero))
    (3 ;(#_QPainter::CompositionMode_Source)
     (gl:blend-equation :src-nv))
    (4 ;(#_QPainter::CompositionMode_Destination)
     (gl:blend-equation :dst-nv))
    (5 ;(#_QPainter::CompositionMode_SourceIn)
     (gl:blend-equation :src-in-nv))
    (6 ;(#_QPainter::CompositionMode_DestinationIn)
     (gl:blend-equation :dst-in-nv))
    (7 ;(#_QPainter::CompositionMode_SourceOut)
     (gl:blend-equation :src-out-nv))
    (8 ;(#_QPainter::CompositionMode_DestinationOut)
     (gl:blend-equation :dst-out-nv))
    (9 ;(#_QPainter::CompositionMode_SourceAtop)
     (gl:blend-equation :src-atop-nv))
    (10 ;(#_QPainter::CompositionMode_DestinationAtop)
     (gl:blend-equation :dst-atop-nv))
    (11 ;(#_QPainter::CompositionMode_Xor)
     (gl:blend-equation :xor-nv))
    (12 ;(#_QPainter::CompositionMode_Plus)
     (gl:blend-func :src-alpha :dst-alpha))
    (13 ;(#_QPainter::CompositionMode_Multiply)
     (gl:blend-equation :multiply-nv))
    (14 ;(#_QPainter::CompositionMode_Screen)
     (gl:blend-equation :screen-nv))
    (15 ;(#_QPainter::CompositionMode_Overlay)
     (gl:blend-equation :overlay-nv))
    (16 ;(#_QPainter::CompositionMode_Darken)
     (gl:blend-equation :darken-nv))
    (17 ;(#_QPainter::CompositionMode_Lighten)
     (gl:blend-equation :lighten-nv))
    (18 ;(#_QPainter::CompositionMode_ColorDodge)
     (gl:blend-equation :colordodge-nv))
    (19 ;(#_QPainter::CompositionMode_ColorBurn)
     (gl:blend-equation :colorburn-nv))
    (20 ;(#_QPainter::CompositionMode_HardLight)
     (gl:blend-equation :hardlight-nv))
    (21 ;(#_QPainter::CompositionMode_SoftLight)
     (gl:blend-equation :softlight-nv))
    (22 ;(#_QPainter::CompositionMode_Difference)
     (gl:blend-equation :difference-nv))
    (23 ;(#_QPainter::CompositionMode_Exclusion)
     (gl:blend-equation :exclusion-nv))))
