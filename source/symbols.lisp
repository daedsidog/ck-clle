(defpackage #:ck.clle.symbols
  (:use #:cl)
  (:export #:defvar*
           #:defparameter*
           #:defconstant*
           #:defgeneric*
           #:defclass*
           #:defun*
           #:defmacro*
           #:defmethod*
           #:export-inherited-symbols
           #:keywordicate))

(in-package #:ck.clle.symbols)

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
  (let* ((exports
           (apply #'append
                  (mapcar (lambda (slot-spec)
                            (let ((should-export t)
                                  (export-specifier (member :export slot-spec)))
                              (when export-specifier
                                (let ((export-opt (second export-specifier)))
                                  (when (and (not (member export-opt '(t nil)))
                                             (cdr export-specifier))
                                    (error "Invalid export specifier for slot ~A in class ~A."
                                           (first slot-spec)
                                           name))
                                  (setf should-export export-opt)))
                              (when should-export
                                (list
                                 (when (member :reader slot-spec)
                                   (second (member :reader slot-spec)))
                                 (when (member :accessor slot-spec)
                                   (second (member :accessor slot-spec)))))))
                          direct-slots))))
    (mapc (lambda (slot-spec) (remf (cdr slot-spec) :export)) direct-slots)
    (setf exports (remove nil exports))
    `(prog1
         (defclass ,name ,direct-superclasses ,direct-slots ,@options)
       (export ',name)
       ,@(mapcar (lambda (export-name)
                   `(export ',export-name))
                 exports))))

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

(defmacro export-inherited-symbols (&rest source-packages)
  "Export the external symbols inherited from SOURCE-PACKAGES to the current package's symbol table.

Exports only if the symbol is accessible in the current package."
  `(progn
     (dolist (source-package ,source-packages)
       (loop for symbol being the external-symbols of source-package
             do (multiple-value-bind (symbol source) (find-symbol (symbol-name symbol) *package*)
                  (unless (eq source :internal)
                    (export symbol ,(package-name *package*))))))
     (loop for symbol being the internal-symbols of *package*
           do (dolist (source-package ,source-packages)
                (when (member (symbol-package symbol) ,source-packages)
                  (export symbol ,(package-name *package*)))))))

(defun keywordicate (symbol)
  "Intern SYMBOL into the keyword package and return it."
  (intern (symbol-name symbol) :keyword))