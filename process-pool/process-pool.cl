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

(defun big-array-handler (in-array out-array
			  start-index end-index
			  &optional (iterations 20)
			  &aux (done 0))
  ;; A function that can run for a measurable amount of time, 
  ;; and one that can operate on a shared data structure in
  ;; several parallel threads.
  ;; 
  ;; This function performs long arithmetic operations on a segment of
  ;; a large array.  Disjoint segments can be processed in parallel
  ;; without interference.
  ;;
  ;; Stay in fixnum operations to avoid consing that could interfere 
  ;; with parallel operation.
  ;; IN-ARRAY and OUT-ARRAY should be arrays of the same size
  ;; whose values are small fixnums or NIL. 
  ;; START-INDEX and END-INDEX are indices into these arrays
  ;; with START-INDEX <= END-INDEX.
  ;; ITERATIONS is a positive integer.
  ;;
  ;; This function iterates ITERATIONS times, modifying
  ;; the chunks of the two arrays from
  ;; START-INDEX to END-INDEX. The actual operations are not
  ;; important: they involve arithmetic calculations with
  ;; some checks to ensure values are suitable. Their purpose
  ;; is to take time so we can compare using one process
  ;; versus several in a pool.
  
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
  (ensure-default-process-pool)  ;; We make sure there is
                                 ;; a default process pool
  (dotimes (i n) ;; We run N pool processes
    (push (multiple-value-list
	   (process-pool-run 
	    nil :function #'big-array-handler
	    :arguments (list in out (* i chunk-width)
			     (min width (* (1+ i) chunk-width)) r)
	    ;; We call BIG-ARRAY-HANDLER with a chunk of the array
	    ;; where the chunk is the (/ array-size n) suitably
	    ;; adjusted to prevent overlap.
	    :data endmark
	    :report-end (lambda (work v err) ;; Report that a pool
                                             ;; process has completed its work.
			  (declare (ignore err))
			  (incf-atomic (car work-done) (car v))
			  (barrier-pass-through
			   (process-pool-work-item-data work)))))
	  *old-work*))
  (barrier-wait endmark) ;; Wait until all process-pool processes have 
                         ;; finished
  work-done)

#|

        ;;; Some sample runs. Timing depends on factors like
        ;;; machine speed and amount of physical memory so you
        ;;; will likely see different timings but the ratio of
        ;;; timings should be similar.

        ;;; A run in one thread to establish a baseline timing.

cl-user(74): (time (simple-parallel-task :n 1 :in *in* :out *out* :r 50))
; cpu time (non-gc) 4.709284 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  4.709284 sec user, 0.000000 sec system
; cpu time (thread) 0.001000 sec user, 0.000000 sec system
; real time  4.708741 sec (100.0%)
; space allocation:
;  153 cons cells, 6,432 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
(50000000)


        ;;; A run in three threads takes about 1/3 of the time.
        ;;; The CPU time is the sum of the times of all processes
        ;;; so since 3 ran, the REAL time is roughly 1/3 CPU time.

cl-user(76): (time (simple-parallel-task :n 3 :in *in* :out *out* :r 50))
; cpu time (non-gc) 4.672289 sec user, 0.000000 sec system
; cpu time (gc)     0.000000 sec user, 0.000000 sec system
; cpu time (total)  4.672289 sec user, 0.000000 sec system
; cpu time (thread) 0.001000 sec user, 0.000000 sec system
; real time  1.592101 sec (293.5%)
; space allocation:
;  317 cons cells, 11,920 other bytes, 0 static bytes
; Page Faults: major: 0 (gc: 0), minor: 0 (gc: 0)
(50000000)


|#
