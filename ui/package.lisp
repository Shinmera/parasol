#|
This file is a part of Parasol
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol)
(defpackage #:parasol-ui
  (:use #:cl #:parasol-dev #:parasol-document #:parasol-tools)
  (:nicknames #:org.shirakumo.parasol.ui)
  ;; document-view.lisp
  (:export
   #:*mouse-pressure*
   #:document-view
   #:document
   #:angle
   #:zoom
   #:mirror-x
   #:mirror-y
   #:pen
   #:pen-pressed)
  ;; gizmo-bar.lisp
  (:export
   #:gizmo-bar)
  ;; main-window.lisp
  (:export
   #:*window*
   #:main-window
   #:tool
   #:current-view
   #:current-document
   #:main)
  ;; tab-area.lisp
  (:export
   #:tab
   #:title
   #:parent
   #:index
   
   #:welcome-tab
   
   #:tab-area
   #:add-tab
   #:change-tab
   #:close-tab
   #:current-tab))
