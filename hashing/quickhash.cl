;; -*- mode: common-lisp; package: excl -*-
;;
;; quickhash.cl
;; quick and dirty hash table, using the new extensible hash-table implementation.
;;
;; This code is in the public domain.
;;

#-(version>= 11 0)
(error "def-hash-table-implementation is only defined in 11.0 and later versions.")

(eval-when (compile) (declaim (optimize (speed 3) (safety 1))))
	   
(in-package :user)


;; The quickhash struct; we will use this struct as the hash-table-instance
;; in the hash-table implementation.
(defstruct quickhash
  data					; vector of data
  mask					; mask key to get hash index
  count					; number of data items
  grow					; grow if more than this many entries
  )

(defun create-quickhash (size rehash-threshold rehash-size value-vector weak-keys)
  (declare (ignore rehash-threshold rehash-size value-vector weak-keys))
  (let* ((table-size (ash 1 (integer-length size)))
	 (qt (make-quickhash
	      :data (make-array table-size :initial-element nil)
	      :mask (1- table-size)
	      :count 0
	      :grow  (+ (ash table-size -1) (ash table-size -2))
	      )))
    qt))

(defun quick-hash-count (hash-table)
  (quickhash-count (ha_instance hash-table)))


(defun get-quick (index ht default)
  (let ((instance (ha_instance ht)))
    (dolist (val (svref (quickhash-data instance) (logand index (quickhash-mask instance))))
      (if* (eql (car val) index)
	 then (return-from get-quick (values (cdr val) t))))
    (values default nil)))

(defun get-quick_2op_1ret (index ht)
  (let ((instance (ha_instance ht)))
    (dolist (val (svref (quickhash-data instance) (logand index (quickhash-mask instance))))
      (if* (eql (car val) index)
	 then (return-from get-quick_2op_1ret (values (cdr val) t))))
    nil))

(defun inv-get-quick (index ht value)
  ;; store value in the quickhash
  ;;
  (let ((instance (ha_instance ht)))
    ;; grow table if necessary
    (when (> (quickhash-count instance) (quickhash-grow instance)) 
      (grow-quick ht)
      (setq instance (ha_instance ht)))
    
    (let* ((data (quickhash-data instance))
	   (ind  (logand index (quickhash-mask instance)))
	   (vals (svref data ind)))
      
      (if* vals
	 then (do* ((prev nil vv)
		    (vv vals (cdr vv))
		    (val (car vv) (car vv)))
		  ((null vv)
		   (setf (cdr prev) (list (cons index value)))
		   (incf (quickhash-count instance)))
		(when (eql (car val) index)
		  ;; replace existing entry
		  (setf (cdr val) value)
		  (return)))
	 else ;; first entry for this index
	      (setf (svref data ind) (list (cons index value)))
	      (incf (quickhash-count instance)))
      value)))
	    
;: free-btree-block-from-cache, write-modified-btree-blocks
;: get-from-class-cache, close-logfile, prune-logfiles, check-for-new-file
;: 
(defun rem-quick (index ht)
  ;; remove item from quickhash
  ;; return t if item found to remove
  (let* ((instance (ha_instance ht))
	 (data (quickhash-data instance))
	 (ind  (logand index (quickhash-mask instance)))
	 (vals (svref data ind)))
    
    (if* vals
       then (do* ((prev nil vv)
		  (vv vals (cdr vv))
		  (val (car vv) (car vv)))
		((null vv)
		 nil)
	      (when (eql (car val) index)
		;; found entry to remove
		(if* prev
		   then (setf (cdr prev) (cdr vv))
		   else (setf (svref data ind) (cdr vv)))
		
		(decf (quickhash-count instance))
		(return t))))))

;: .inv-get-quick
;: 
(defun grow-quick (ht)
  ;; double the size of the quickhash
  (let* ((instance (ha_instance ht))
	 (odata (quickhash-data instance))
	 (osize (length odata))
	 (nsize (ash osize 1))
	 (data (make-array nsize :initial-element nil))
	 (nmask (1- nsize))
	 )
    
    (dotimes (i osize)
      (let ((vals (svref odata i)))
	(if* vals
	   then ;; values will either stay in this bucket or
		;; move to the bucket osize higher
		(do ((prev nil)
		     (head nil)
		     (vv vals (cdr vv))
		     (val nil)
		     (nvals)
		     )
		    ((null vv)
		     (if* head
			then (setf (svref data i) head))
		     (if* nvals
			then (setf (svref data (+ i osize)) nvals)) 
		     )
		  
		  (setq val (car vv))
		  (let ((nloc (logand (car val) nmask)))
		    (if* (not (eq i nloc))
		       then ; move this entry
			    (push val nvals)
			    (if* prev
			       then (setf (cdr prev) (cdr vv)))
		       else ; entry stays
			    (if* (null head) 
			       then (setq head vv))
			    (setq prev vv)))))))
    
    (setf (quickhash-data instance) data)
    (setf (quickhash-mask instance) nmask)
    (setf (quickhash-grow instance) (+ osize (ash osize -1)))
    
    ht))

(defun quick-hash-iterate (instance state)
  (unless state
    (setq state (cons -1 nil)))
  (let* ((index (car state))
	 (ent (cdr state))
	 (data (quickhash-data instance))
	 (len (length (the (simple-array t ()) data))))
    (loop
      (if* ent
	 then (setf (car state) index
		    (cdr state) (cdr ent))
	      (return-from quick-hash-iterate
		(values state (caar ent) (cdar ent)))
	 else (incf index)
	      (when (>= index len)
		(return-from quick-hash-iterate nil))
	      (setq ent (svref data index))))))

	
(def-hash-table-implementation :quickhash
    :based-on nil ;; Start from scratch, with no :acl functionality
    :new-instance #'create-quickhash
    :count #'quick-hash-count
    :iterate #'quick-hash-iterate
    :inv-gethash #'inv-get-quick
    :gethash #'get-quick
    :remhash #'rem-quick
    :gethash-2op-1ret #'get-quick_2op_1ret)


(defun test-quick ()
  (let ((keys))
  (dotimes (i 30000)
    (pushnew (random 1000000000) keys))
  (let ((kvs))
    (dolist (k keys)
      (push (cons k (random 5000000)) kvs))
    (format t "~s keys~%" (length kvs))
    
    
    (let ((qt (make-hash-table :size 20 :implementation :quickhash)))
      (dolist (kv kvs)
	(setf (gethash (car kv) qt) (cdr kv)))
      
      
      (dolist (kv kvs)
	(if* (not (eql (cdr kv) (gethash (car kv) qt)))
	   then (error "no match")))
      qt))))
    
(defun analyze-quickhash (hash-table)
  (let ((totallen 0)
	(max-len 0)
	(data (quickhash-data (ha_instance hash-table))))
    (dotimes (i (length data))
      (let ((len (length (svref data i))))
	(incf totallen len)
	(setq max-len (max max-len len))))
    
    (format t "~s entries, total items: ~s, avg len ~s,  max len ~s~%"
	    (length data)
	    totallen
	    (/ totallen (float (length data)))
	    max-len)))
