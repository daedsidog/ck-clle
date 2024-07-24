(defpackage #:ck.clle.symbols
  (:use #:cl)
  (:export #:export-inherited-symbols
           #:keywordicate))

(in-package #:ck.clle.symbols)

(defmacro export-inherited-symbols (&rest source-packages)
  "Export the external symbols inherited from SOURCE-PACKAGES to the current package's symbol table.

Exports only if the symbol is accessible in the current package."
  `(progn
     (dolist (source-package ',source-packages)
       (loop for symbol being the external-symbols of source-package
             do (multiple-value-bind (symbol source) (find-symbol (symbol-name symbol) *package*)
                  (unless (eq source :internal)
                    (export symbol ,(package-name *package*))))))
     (do-symbols (symbol *package*)
       do (dolist (source-package ',source-packages)
            (setf source-package (find-package source-package))
            (when (eq (symbol-package symbol) source-package)
              (export symbol ,(package-name *package*)))))))

(defun keywordicate (symbol)
  "Intern SYMBOL into the keyword package and return it."
  (intern (symbol-name symbol) :keyword))
