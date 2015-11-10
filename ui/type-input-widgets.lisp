#|
 This file is a part of Parasol
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.parasol.ui)
(named-readtables:in-readtable :qtools)

(defvar *type-class-map* (make-hash-table :test 'eql))

(defun make-input-for-type (type object slot &optional (value NIL v-p))
  (let ((class (gethash (if (listp type) (first type) type) *type-class-map*)))
    (if v-p
        (make-instance class :object object :slot slot :constraint type :value value)
        (make-instance class :object object :slot slot :constraint type))))

;;;;;
;; Additional types to distinguish
(deftype text ()
  `string)

(deftype range (from to)
  `(float ,from ,to))

;;;;;
;; Basic setters
(define-widget slot-setter (QWidget)
  ((value :accessor value)
   (object :initarg :object :accessor object)
   (slot :initarg :slot :accessor slot)
   (constraint :initarg :constraint :accessor constraint))
  (:default-initargs
    :object (error "OBJECT required.")
    :slot (error "SLOT required.")
    :constraint (error "CONSTRAINT required.")))

(defmethod initialize-instance :after ((setter slot-setter) &key (value NIL v-p) &allow-other-keys)
  (when v-p
    (setf (value setter) value)))

(defgeneric (setf value) (new-val slot-setter)
  (:method :around (new-val (setter slot-setter))
    (when (or (not (slot-boundp setter 'value))
              (not (equal new-val (value setter))))
      (call-next-method))
    new-val)
  (:method (new-val (setter slot-setter))
    (setf (q+:value setter) new-val))
  (:method :after (new-val (setter slot-setter))
    (setf (slot-value setter 'value) new-val)
    (when (or (not (slot-boundp (object setter) (slot setter)))
              (not (equal new-val (slot-value (object setter) (slot setter)))))
      (setf (slot-value (object setter) (slot setter)) new-val))))

(define-widget ranged-setter (QWidget slot-setter)
  ((maximum :initform NIL :accessor maximum)
   (minimum :initform NIL :accessor minimum)))

(defmethod (setf maximum) (value (setter ranged-setter))
  (setf (q+:maximum setter) value))

(defmethod (setf minimum) (value (setter ranged-setter))
  (setf (q+:minimum setter) value))

(defmethod initialize-instance :after ((setter ranged-setter) &key)
  (let ((constraint (constraint setter)))
    (when (listp constraint)
      (when (second constraint)
        (setf (minimum setter) (second constraint)))
      (when (third constraint)
        (setf (maximum setter) (third constraint))))))

(defmacro define-type-input (name (qclass &rest direct-superclasses) direct-slots &rest options)
  (destructuring-bind (name atomic-type) name
    `(progn
       (setf (gethash ',atomic-type *type-class-map*) ',name)
       (define-widget ,name (,qclass ,@direct-superclasses)
         ,direct-slots
         ,@options))))

(indent:define-indentation define-type-input
    (4 (&whole 6 &rest)
       (&whole 2 (&whole 0 0 &rest 2))
       &rest (&whole 2 2 &rest (&whole 2 2 4 &body))))

;;;;;
;; Container
(define-type-input (widget-setter widget) (QWidget slot-setter)
  ())

(define-subwidget (widget-setter layout) (q+:make-qvboxlayout widget-setter)
  (q+:add-widget layout (slot-value (object widget-setter) (slot widget-setter))))

;;;;;
;; Integer
(define-type-input (integer-setter integer) (QSpinBox ranged-setter)
  ())

(define-slot (integer-setter changed) ((new-val int))
  (declare (connected integer-setter (value-changed int)))
  (setf (value integer-setter) new-val))

;;;;;
;; Range
(define-type-input (range-setter range) (QSlider ranged-setter)
  ())

(define-slot (range-setter changed) ((new-val int))
  (declare (connected range-setter (value-changed int)))
  (setf (value range-setter) new-val))

;;;;;
;; Float
(define-type-input (float-setter float) (QDoubleSpinBox ranged-setter)
  ())

(define-slot (float-setter changed) ((new-val double))
  (declare (connected float-setter (value-changed double)))
  (setf (value float-setter) new-val))

;;;;;
;; Boolean
(define-type-input (boolean-setter boolean) (QPushButton slot-setter)
  ())

(define-initializer (boolean-setter setup)
  (setf (q+:checkable boolean-setter) T))

(define-slot (boolean-setter changed) ((new-val bool))
  (declare (connected boolean-setter (toggled bool)))
  (setf (value boolean-setter) new-val))

(defmethod (setf value) (new-val (setter boolean-setter))
  (setf (q+:down setter) new-val))

;;;;;
;; String
(define-type-input (string-setter string) (QLineEdit slot-setter)
  ())

(define-slot (string-setter changed) ()
  (declare (connected string-setter (editing-finished)))
  (setf (value string-setter) (q+:text string-setter)))

(defmethod (setf value) (new-val (setter string-setter))
  (setf (q+:text setter) new-val))

;;;;;
;; Text
(define-type-input (text-setter text) (QTextEdit slot-setter)
  ())

(define-slot (text-setter changed) ()
  (declare (connected text-setter (text-changed)))
  (setf (value text-setter) (q+:to-plain-text text-setter)))

(defmethod (setf value) (new-val (setter text-setter))
  (setf (q+:plain-text setter) new-val))

;;;;;
;; Color
(define-type-input (color-setter color) (QPushButton slot-setter)
  ())

(define-initializer (color-setter setup)
  (setf (q+:flat color-setter) T))

(defmethod (setf value) (new-val (setter color-setter))
  (let ((pal (q+:palette setter)))
    (setf (q+:color pal (q+:qpalette.button)) (to-qcolor new-val))
    (setf (q+:palette setter) pal)
    (setf (q+:auto-fill-background setter) T)
    (q+:update setter)))

(define-slot (color-setter changed) ()
  (declare (connected color-setter (clicked)))
  (let ((color (q+:qcolordialog-get-color (to-qcolor (value color-setter)) color-setter)))
    (when (q+:is-valid color)
      (setf (value color-setter)
            (make-instance 'color
                           :r (q+:red color)
                           :g (q+:green color)
                           :b (q+:blue color)
                           :a (q+:alpha color))))))

;;;;;
;; Member
(define-type-input (member-setter member) (QComboBox slot-setter)
  ())

;; We need this to ensure that we don't switch already on init
;; when the default value is about to be set.
(defvar *init* NIL)
(define-initializer (member-setter setup)
  (let ((*init* T))
    (dolist (item (cdr (constraint member-setter)))
      (q+:add-item member-setter (princ-to-string item)))))

(define-slot (member-setter changed) ((new-val int))
  (declare (connected member-setter (current-index-changed int)))
  (unless *init*
    (setf (value member-setter)
          (nth new-val (cdr (constraint member-setter))))))

(defmethod (setf value) (new-val (setter member-setter))
  (let ((pos (position new-val (cdr (constraint setter)))))
    (cond ((not pos)
           (v:warn :parasol-ui "Attempting to set value ~s, which is not a member of ~s"
                   new-val setter))
          ((or (not (q+:current-index setter))
               (not (= pos (q+:current-index setter))))
           (setf (q+:current-index setter) pos)))))

;;;;;
;; Configurable
(define-type-input (configurable-setter configurable) (QWidget)
  ((object :initarg :object :accessor object)))

(define-subwidget (configurable-setter layout) (q+:make-qvboxlayout configurable-setter)
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 0)
  (dolist (name (configurable-slots object))
    (let ((slot (find name (c2mop:class-slots (class-of object))
                      :key #'c2mop:slot-definition-name)))
      (when slot
        (let ((type (c2mop:slot-definition-type slot)))
          (case type
            (configurable
             (when (slot-boundp object name)
               (q+:add-widget layout (make-input-for-configurable (slot-value object name)))))
            (T
             (q+:add-widget
              layout
              (if (slot-boundp object name)
                  (make-input-for-type type object name (slot-value object name))
                  (make-input-for-type type object name))))))))))

(defmethod finalize :before ((setter configurable-setter))
  (sweep-layout (slot-value setter 'layout)))

(defun make-input-for-configurable (configurable)
  (make-instance 'configurable-setter :object configurable))
