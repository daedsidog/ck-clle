(defpackage #:ck.clle.exportation
  (:use #:cl)
  (:export #:defvar* #:defparameter* #:defconstant* #:defgeneric* #:defclass* #:defun* #:defmacro*
           #:defmethod* #:export*))

(in-package #:ck.clle.exportation)

(defmacro defvar* (name &optional (init nil init-provided-p) doc)
  "Define a global variable and export it.  Optionally provide an initial value and documentation
string."
  `(progn
     (defvar ,name ,@(when init-provided-p (list init)) ,doc)
     (export ',name)))

(defmacro defparameter* (name value &optional doc)
  "Define a global parameter with a value and export it.  Optionally provide a documentation
string."
  `(progn
     (defparameter ,name ,value ,doc)
     (export ',name)))

(defmacro defconstant* (name value &optional doc)
  "Define a global constant with a value and export it.  Optionally provide a documentation string."
  `(progn
     (defconstant ,name ,value ,doc)
     (export ',name)))

(defmacro defgeneric* (name lambda-list &rest options-and-doc)
  "Define a generic function with a given lambda list and export it.  Optionally include options
and a documentation string."
  `(progn
     (defgeneric ,name ,lambda-list ,@options-and-doc)
     (export ',name)))

(defmacro defclass* (name direct-superclasses direct-slots &rest options)
  "Define a class with given superclasses and slots, export it, and optionally export its accessors.

Accessors are exported by default.  To change this, use :EXPORT NIL for an accessor you do not
want to be exported."
  (let ((exports (apply #'append
                        (mapcar (lambda (slot-spec)
                                  (list
                                   (when (member :reader slot-spec)
                                     (second (member :reader slot-spec)))
                                   (when (member :accessor slot-spec)
                                     (second (member :accessor slot-spec)))))
                                direct-slots))))
    `(progn
       (defclass ,name ,direct-superclasses ,direct-slots ,@options)
       (export ',name)
       ,@(mapcar (lambda (export-name)
                   `(export ',export-name))
                 (remove nil exports)))))

(defmacro defun* (name lambda-list &body body)
  "Define a function with a given lambda list and export it.  Include the body of the function."
  `(progn
     (defun ,name ,lambda-list ,@body)
     (export ',name)))

(defmacro defmacro* (name lambda-list &body body)
  "Define a macro with a given lambda list and export it.  Include the body of the macro."
  `(progn
     (defmacro ,name ,lambda-list ,@body)
     (export ',name)))

(defmacro defmethod* (name (&rest lambda-list) &body body)
  "Define a method with a given lambda list and export it.  Include the body of the method."
  `(progn
     (defmethod ,name ,lambda-list
       ,@body)
     (export ',name)))

(defmacro export* (&rest source-packages)
  "Export the public symbols of SOURCE-PACKAGES to the current package's symbol table."
  `(dolist (source-package ',source-packages)
     (loop for symbol being the external-symbols of source-package
           do (export symbol ,(package-name *package*)))))
