;; -*- mode: common-lisp; package: excl -*-
;;
;; alisthash.cl
;; alist hash table: implement a hash-table with a pure alist structure.
;;
;; This code is in the public domain.
;;

#-(version>= 11 0)
(error "def-hash-table-implementation is only defined in 11.0 and later versions.")

(in-package :user)

(defun make-alist-hash-instance (&rest args)
  (declare (ignore args))
  ;; This vector will hold the alist, which starts empty:
  (vector nil))

(defun alist-hash-count (hash-table)
  (length (svref (ha_instance hash-table) 0)))

(defun alist-hash-iterate (instance state)
  ;; The state starts out nil and ends up nil, and is the cons which holds the
  ;; current key/value pair.
  (if state
      (pop state)
    (setq state (svref instance 0)))
  (values state (caar state) (cdar state)))

(defun get-alist-hash-gethash-fcn (hash-fcn test)
  (declare (ignore hash-fcn))
  #'(lambda (key hash-table default)
      (let* ((instance (ha_instance hash-table))
	     (list (svref instance 0))
	     (ent (assoc key list :test test)))
	(if* ent
	   then (values (cdr ent) t)
	   else (values default nil)))))

(defun get-alist-hash-inv-gethash-fcn (hash-fcn test)
  (declare (ignore hash-fcn))
  #'(lambda (key hash-table value)
      (let* ((instance (ha_instance hash-table))
	     (list (svref instance 0))
	     (ent (assoc key list :test test)))
	(if* ent
	   then (setf (cdr ent) value)
	   else (push (cons key value) (svref instance 0))
		value))))

;; Note that the :gethash and  :inv-gethash functions are specified to
;; receive hash-fcn and test arguments, in order to create closures
;; (rather than each  just naming a function to use in the decriptor vector).
(def-hash-table-implementation :alist-hash
    :based-on nil ;; Start from scratch, with no :acl functionality
    :new-instance #'make-alist-hash-instance
    :count #'alist-hash-count
    :iterate #'alist-hash-iterate
    :inv-gethash get-alist-hash-inv-gethash-fcn
    :gethash get-alist-hash-gethash-fcn
    )
