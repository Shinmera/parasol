#|
 This file is a part of Parasol
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:parasol)

;; Common accessors
(defgeneric background (object)
  (:documentation "Retrieve the object's background property.
This is usually a QBrush object. SETF-able."))

(defgeneric name (object)
  (:documentation "Returns the name of the object as a string."))

(defgeneric (setf background) (value object)
  (:documentation "Set the background of an object.
Acceptable values should be a list of R G B values,
a possible QBrush constructor argument or a pathname
to a texture."))

(defgeneric size (object)
  (:documentation "Returns the relative sizing of the object.
This can be a property related to the object's displaying or an active property
that affects other drawing operations."))

(defgeneric pixmap (object)
  (:documentation "A QPixmap of buffered draw information about the object.
This draw information may be stale. You should relay on DRAW instead of using this directly."))

(defgeneric parent (object)
  (:documentation "Returns the relative parent of the object.
Note that not all objects keep track of their parent, even if they are strictly related."))

;; Common functions
(defgeneric add-layer (object &key name mode)
  (:documentation "Add a new layer with NAME and MODE.
NAME defaults to \"Layer D\" where D is the current count of layers.
MODE defaults to SOURCE-OVER."))

(defgeneric remove-layer (object &optional index)
  (:documentation "Remove a layer."))

(defgeneric activate-layer (object index)
  (:documentation ""))

(defgeneric move-layer (object index)
  (:documentation "Moves the currently active layer to index, pushing the others out of the way."))

(defgeneric draw (object painter)
  (:documentation "Draws the OBJECT using the PAINTER."))

(defgeneric draw-incremental (object painter)
  (:documentation "Only draws new, incremental changes of OBJECT using the PAINTER.
If a complete redraw is necessary, use DRAW instead."))

(defgeneric make-active (object)
  (:documentation "Makes the given object 'active'.
This usually means that the object is set as the new target of input events."))

(defgeneric finalize (object)
  (:documentation "Perform cleanup on the object, making it ready to be GC-ed.
Note that the object and all its content will become unusable after calling this function.
This function is cascading, meaning that complex inner objects automatically receive a
FINALIZE call as well, making them too unsafe to use.

Unless overridden, this is a NO-OP.")
  (:method (object))
  (:method :before (object)
    (format T "~& Finalizing: ~a~%" object)))

(defgeneric destroy (object)
  (:documentation "Requests that the object be destroyed.
If this returns NIL, the object refuses the destruction due to some complication.
Destruction can only proceed if a non-NIL value is returned and the object FINALIZEs itself.

Unless overridden, this simply calls FINALIZE and returns T.")
  (:method (object)
    (finalize object)
    T))
