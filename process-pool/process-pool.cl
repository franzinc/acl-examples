;; A simple process-pool usage example.
;;
;; This code is in the public domain.

(in-package :user)

(eval-when (compile load eval)
  (require :process-pool)
  (require :smputil)
  (use-package :mp))

(defparameter *in* (make-array 1000000))
(defparameter *out* (make-array 1000000))
(defvar *old-work* nil)

(defun big-array-handler (in-array out-array start-index end-index &optional (iterations 20)
			  &aux (done 0))
  ;; A function that can run for a measurable amount of time, 
  ;; and one that can operate on a shared data structure in
  ;; several paallel threads.
  ;; 
  ;; This function performs long arithmetic operations on a segment of
  ;; a large array.  Disjoint segments can be processed in parallel
  ;; without interference.
  ;;
  ;; Stay in fixnum operations to avoid consing that could interfere 
  ;; with parallel operation.
  
  (dotimes (i iterations done)
    (dotimes (j (- end-index start-index))
      (let ((in (aref in-array (+ start-index j)))
	    (out (aref out-array (+ start-index j))))
	(if (or (null in) (eql in 0))
	    (setf (aref in-array (+ start-index j))
	      (setq out (truncate end-index (1+ i))))
	  (setq out (* (truncate in (1+ i)) (* i in))))
	(setf (aref out-array (+ start-index j)) out)
	(incf done)))))

(defun simple-parallel-task (&key n in out (r 20)
			     &aux
			     (endmark (make-barrier (1+ n)))
			     (width (length in))
			     (chunk-width (ceiling width n))
			     (work-done (list 0)))
  (ensure-default-process-pool)
  (dotimes (i n)
    (push (multiple-value-list
	   (process-pool-run 
	    nil :function #'big-array-handler
	    :arguments (list in out (* i chunk-width) (min width (* (1+ i) chunk-width)) r)
	    :data endmark
	    :report-end (lambda (work v err)
			  (declare (ignore err))
			  (incf-atomic (car work-done) (car v))
			  (barrier-pass-through (process-pool-work-item-data work)))))
	  *old-work*))
  (barrier-wait endmark)
  work-done)
