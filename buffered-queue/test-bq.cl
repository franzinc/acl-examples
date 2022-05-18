
(in-package :user)

(eval-when (compile load eval)
  (require :smputil)
  (require :tester)
  )

(defpackage :user (:use :mp :util.test))

(defun real-time-delta (&optional starttime endtime)
  (- (or endtime (get-internal-real-time)) (or starttime 0)))


(defmacro with-time-delta ((timevar) &rest body &aux (v1 (gensym)))
  `(let ((,v1 (real-time-delta)))
     (unwind-protect
	 (progn ,@body)
       (setf ,timevar (real-time-delta ,v1)))))

;; Simple test of the implementation.
(defun test-buffered-queue (&aux
			    q)
  (setq q (make-instance 'buffered-two-actor-queue :buffer-size 10))
  (test t (queue-empty-p q))
  (dotimes (i 10)
    (test i (queue-length q))
    (test t (enqueue q i)))
  (test t (queue-full-p q))
  (test t (enqueue-wait q))
  (setf (enqueue-wait q) 0)
  (test nil (enqueue q 17))
  )
		       
;; Compare the performance of two queue implementations
;; by measuring the time to run an wnqueu/dequeue pair
;; in the same thread.
(defun test-simple-queue-pair (&key
			       (q1 'queue)
			       (init1 nil)
			       (q2 'buffered-two-actor-queue)
			       (init2 '(:buffer-size 1000 
					:name "tbq" :enqueue-wait t))
			       (n 1000000)
			       &aux t1 t2)
  (setq q1 (apply 'make-instance q1 init1))
  (setq q2 (apply 'make-instance q2 init2))
  (flet ((dorun (q &aux time)
	   (with-time-delta (time)
	     (dotimes (i n)
	       (enqueue q i)
	       (dequeue q)))
	     time))
    (setq t1 (dorun q1))
    (setq t2 (dorun q2))
    (format t "~&test-simple-queue-pair ~A ~A ~A~%" t1 t2 (/ (float t1) t2))
    (test t1 t2 :test #'>)))

;; Measure the performance of a queue implementation by
;; running provider and consumer in separate threads
;; using a very naive pattern of usage.
;;
;; A more comples usage pattern could be simulated by introducing
;; delays between enqueue and dequeue calls, but such a test is
;; likely to run considerably longer to collect meaningful statistics.

(defun test-queue-perf (&key (queue-type 'buffered-two-actor-queue)
			     (initargs (case queue-type
					 ('buffered-two-actor-queue
					  '(:buffer-size 1000 
					    :name "tbq" :enqueue-wait t))
					 (otherwise nil)))				       
			     (item-count 20000000)  ;;; Max items or nil.
			     (time-limit 60)	    ;;; Max seconds or nil.
			     (check-ordering t)
			     (wait t)
			     provider-delay
			     provider-group
			     consumer-delay
			     consumer-group
			     progress &aux (interval 10))
  (format t "~&; test-queue-perf ~S ~S :item-count ~A  time-limit ~A~%"
	  queue-type initargs item-count time-limit)
  (when (numberp progress) (setq interval progress))
  (let* ((queue (apply #'make-instance queue-type initargs))
	 start-time
	 (starter (make-barrier 3 :callback (lambda (b)
					      (declare (ignore b))
					      (setq start-time (get-internal-real-time))
					      )))
	 (done-gate (mp:make-gate nil))
	 (pr (list 0))
	 (end-time (when time-limit (+ (get-internal-real-time) (* time-limit 1000))))
	 run-time pc ps)
    (macrolet ((set-group-and-delay (basegroup basedelay)
		 `(progn
		    (setq randomize-group (and ,basegroup (< ,basegroup 0)))
		    (setq group (and ,basegroup (abs ,basegroup)))
		    (setq this-group group)
		    (setq randomize-delay (and ,basedelay (< ,basedelay 0)))
		    (setq delay (and ,basedelay (abs ,basedelay)))
		    (setq this-delay delay)))
	       (do-group ()
		 `(cond ((null delay))
			((and this-group)
			 (> this-group (1+ (mod i group))))
			((< this-delay 0.1) (process-allow-schedule))
			(t (sleep this-delay)
			   (when randomize-group
			     (setq this-group (1+ (random group))))
			   (when randomize-delay
			     (setq this-delay (* delay (1+ (random 100)) 0.01)))))))
      (setq pc
	(mp:process-run-function
	    (format nil "~A" (gensym "queue-consumer"))
	  (lambda ()
	    (when progress (format t "~&;Started ~S~%" *current-process*))
	    (barrier-wait starter)
	    (unwind-protect
		(let* (value randomize-group group this-group randomize-delay delay this-delay)
		  (set-group-and-delay consumer-group consumer-delay)
		  (when progress (format t "~&;Starting consumer loop.~%"))
		  (dotimes (i (or item-count most-positive-fixnum)
			     (when progress (format t "~&;Consumer loop item-count reached ~S.~%" value)))
		    (setq value (dequeue queue :wait 5))
		    (when (null value)
		      (format t "~&;Ending consumer early.~%")
		      (return))
		    (incf (car pr))
		    (when check-ordering (or (eql i value) (error "bad sequence")))
		    (do-group)
		    ))
	      (mp:open-gate done-gate)
	      (when progress (format t "~&;Consumer done.~%"))
	      (ignore-errors (mp:process-kill ps))))))
      (setq ps
	(mp:process-run-function
	    (format nil "~A" (gensym "queue-provider"))
	  (lambda ()
	    (when progress (format t "~&;Started ~S~%" *current-process*))
	    (barrier-wait starter)
	    (unwind-protect 
		(let (randomize-group group this-group randomize-delay delay this-delay)
		  (set-group-and-delay provider-group provider-delay)
		  (when progress (format t "~&;Starting provider loop.~%"))
		  (dotimes (i (or item-count most-positive-fixnum))
		    (enqueue queue i)
		    (do-group)
		    ))
	      (when progress (format t "~&;Provider done.~%"))))))
      (mp:process-run-function 
	  (format nil "~A" (gensym "queuw-nanny"))
	(lambda ()
	  (loop
	    (sleep interval)
	    (cond ((mp:gate-open-p done-gate) (return))
		  ((null end-time))
		  ((< end-time (get-internal-real-time))
		   (ignore-errors (process-kill pc))
		   (return)))
	    (when progress (format t "~&; occupancy: ~S after ~A items~%" (queue-length queue) pr))
	    )))
      (barrier-wait starter)
      (when wait
	(mp:process-wait "Waiting for queues" #'mp:gate-open-p done-gate))
      (setq run-time (- (get-internal-real-time) start-time))
      (format t "~&; test-queue-perf time ~A items ~A~%" run-time (car pr))
      (values run-time (car pr)))))
  

(defun test-queue-pair (&key (q1 'queue)
			     (init1 nil)
			     (q2 'buffered-two-actor-queue)
			     (init2 '(:buffer-size 1000 
				      :name "tbq" :enqueue-wait t))
			     (wait t)
			     (n 20000000)     ;;; Max item-count or nil.
			     (time 60)	      ;;; Max seconds or nil.
			     progress provider-group provider-delay consumer-group consumer-delay
			&aux q1msec q1n q2msec norm1 norm2 q2n ratio sn)
  (unwind-protect
      (flet ((safe/ (x y) (if (or (null y) (eql y 0)) 0 (/ x y))))
	(multiple-value-setq (q1msec q1n)
	  (test-queue-perf :queue-type q1 :initargs init1 :item-count n :time-limit time
			   :wait wait :progress progress
			   :provider-group provider-group :provider-delay provider-delay
			   :consumer-group consumer-group :consumer-delay consumer-delay
			   ))
	(setq norm1 (round (safe/ (* 1000000.0 q1msec) q1n)))
	(multiple-value-setq (q2msec q2n)
	  (test-queue-perf :queue-type q2 :initargs init2 :item-count n :time-limit time
			   :provider-group provider-group :provider-delay provider-delay
			   :consumer-group consumer-group :consumer-delay consumer-delay
			   :wait wait :progress progress))
	(setq norm2 (round (safe/ (* 1000000.0 q2msec) q2n)))
	(setq ratio (safe/ (float norm1) norm2))
	(format t "~&; test-queue-pair norm=time-per-million-tems ~S ~S~%" norm1 norm2)
	(format t "~&; test-queue-pair time ratio norm1/norm2: ~S~%" ratio)
	(test 1 ratio :test #'<)
	)
    (format t "~&test-queue-pair summary:~%")
    (format 
     t 
     "~&Host  ACL mode         Count Run ms Per 1M         Count Run ms Per 1M      Ratio~%")
    (format t "~&~8A ~A~A  q1 ~9D ~6D ~6D  q2 ~9D ~6D ~6D   ~8,4F ~%"
	    (subseq (setq sn (short-site-name)) 0 (position #\. sn))
	    #+smp "SMP" #-smp "gmp" #+64bit 64 #-64bit 32
	    (or q1n 0) (or q1msec 0) norm1
	    (or q2n 0) (or q2msec 0) norm2
	    ratio
	    )
    ))


  
