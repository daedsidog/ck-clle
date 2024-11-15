(defpackage #:ck-clle/list
  (:use #:cl)
  (:import-from #:alexandria #:flatten)
  (:export #:flatten
           #:duplicates
           #:unique
           #:cars
           #:deep-mapl))

(in-package #:ck-clle/list)

(defun duplicates (data-list &optional &key (test #'eq))
  "Return a list of duplicate elements in DATA-LIST."
  (check-type data-list list)
  (let ((hash (make-hash-table :test test)))
    (loop for x in data-list
          when (= (incf (gethash x hash 0)) 2)
            collect x into dupes
          finally (return dupes))))

(defun unique (data-list &optional &key (test #'eq))
  "Return a list of unique elements in DATA-LIST."
  (let ((hash (make-hash-table :test test)))
    (loop for x in data-list
          when (= (incf (gethash x hash 0)) 1)
            collect x into uniques
          finally (return uniques))))

(defun cars (list)
  "Return the CAR of each nonempty list in LIST."
  (let ((is-first-iteration t))
    (let ((cars (loop for item in list
                      if (listp item)
                        collect (nreverse (cars item)) into cars
                      else
                        when is-first-iteration
                          collect item into cars
                          and do (setf is-first-iteration nil)
                      finally (return cars))))
      (flatten cars))))

(defun deep-mapl (fn list)
  "Apply FN to all sublists in LIST and return LIST.

Recursive version of CL:MAPL."
  (check-type fn function)
  (flet ((helper (list)
           (iterate (for item in list)
             (when (listp item)
               (deep-mapl fn item)
               (mapl fn item)))))
    (helper list))
  list)
