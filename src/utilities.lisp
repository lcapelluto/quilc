;;;; utilities.lisp
;;;;
;;;; Initial author: Eric Peterson

(in-package #:cl-quil)

(defun required-slot (slot-name)
  (check-type slot-name symbol)
  (error "The slot named ~S is required." slot-name))

(defmacro postpend (obj place)
  `(if ,place
       (push ,obj (cdr (last ,place)))
       (setf ,place (list ,obj))))

(defun make-adjustable-vector ()
  (make-array 4 :element-type t
                :initial-element nil
                :adjustable t
                :fill-pointer 0))

(defun vnth (index vector)
  "Like NTH, but for VECTORs."
  (aref vector index))

(defun (setf vnth) (val index vector)
  (setf (aref vector index) val))

(defmacro dohash (((key val) hash) &body body)
  `(maphash (lambda (,key ,val) ,@body)
            ,hash))

(defmacro define-global-counter (counter-name incf-name)
  `(progn
     (declaim (type fixnum ,counter-name))
     (global-vars:define-global-var ,counter-name 0)
     (defun ,incf-name ()
       #+sbcl
       (sb-ext:atomic-incf ,counter-name)
       #+lispworks
       (system:atomic-incf ,counter-name)
       #-(or sbcl lispworks)
       (incf ,counter-name))))

(defun first-column-operator= (mat1 mat2)
  (multiple-value-bind (mat1 mat2) (quil::matrix-rescale mat1 mat2)
    (setf mat1 (quil::scale-out-matrix-phases mat1 mat2))
    (quil::matrix-first-column-equality mat1 mat2)))

(defun operator= (mat1 mat2)
  (multiple-value-bind (mat1 mat2) (quil::matrix-rescale mat1 mat2)
    (setf mat1 (quil::scale-out-matrix-phases mat1 mat2))
    (quil::matrix-equality mat1 mat2)))

(defun matrix-equals-dwim (mat1 mat2)
  "Returns true if mat1 is equal to mat2, with the specific notion of equality
depending on whether *ENABLE-STATE-PREP-COMPRESSION* is enabled."
  (funcall (if quil::*enable-state-prep-compression*
               #'first-column-operator=
               #'operator=)
           mat1
           mat2))
