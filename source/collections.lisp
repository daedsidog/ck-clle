(defpackage #:ck.clle.collections
  (:use #:cl #:ck.clle.exportation #:iterate))

(in-package #:ck.clle.collections)

;;; LIST EXTENSIONS

(defun* duplicates (data-list &optional &key (test #'eq))
  "Return a list of duplicate elements in DATA-LIST."
  (check-type data-list list)
  (let ((hash (make-hash-table :test test)))
    (loop for x in data-list
          when (= (incf (gethash x hash 0)) 2)
            collect x into dupes
          finally (return dupes))))

(defun* unique (data-list &optional &key (test #'eq))
  "Return a list of unique elements in DATA-LIST."
  (let ((hash (make-hash-table :test test)))
    (loop for x in data-list
          when (= (incf (gethash x hash 0)) 1)
            collect x into uniques
          finally (return uniques))))

(defun* flatten (list)
  "Flatten a LIST of sublists into a single list."
  (labels ((flatten-helper (list acc)
           (cond
             ((null list) acc)
             ((atom (car list)) (flatten-helper (cdr list) (cons (car list) acc)))
             (t (flatten-helper (car list) (flatten-helper (cdr list) acc))))))
    (nreverse (flatten-helper list nil))))

(defun* cars (list)
  "Return the CAR of each nonempty list in LIST."
  (check-type list list)
  ;; If we got a list, we need to collect its CAR.  Then, we continue iterating on the rest of the
  ;; list to see if there are any more nested lists, and repeat the process.
  (let ((cars (iterate (for item in list)
                      (if (listp item)
                          (collect (cars item))
                          (when (first-iteration-p)
                            (collect item))))))
    (flatten cars)))