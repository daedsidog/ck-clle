(defpackage #:ck.clle.collections
  (:use #:cl #:ck.clle.exportation))

(in-package #:ck.clle.collections)

;;; LIST EXTENSIONS

(defun* duplicates (data-list &optional &key (test #'eq))
  (check-type data-list list)
  (let ((hash (make-hash-table :test test)))
    (loop for x in data-list
          when (= (incf (gethash x hash 0)) 2)
            collect x into dupes
          finally (return dupes))))
