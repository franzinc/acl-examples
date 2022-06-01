
(in-package :user)


;;; buffered-two-actor-queue
;;;
;;; The code is based on sources contributed by RavenPack
;;; with the following release:
;;;
;;;  >>The code was written by a RavenPack employee on company time
;;;  >>and is generally subject to RavenPack copyright.
;;;  >>
;;;  >>In this case, we freely allow Franz to use or modify the
;;;  >>code, with no requirement to owe anything to RavenPack.
;;;  >>Further, you may re-license the code in any way you see fit.
;;;  >>If you want to give us credit in a comment, that’s fine but 
;;;  >>not even required.
;;;  >>
;;;  >>Jason S. Cornez
;;;  >>CTO, RavenPack
;;
;; buffered-two-actor-queue    (WAS Length limited SPSC queue)   
;;
;; A queue with a fixed length buffer for entries, restricted to a
;; single provider and a single consumer.  As long as the buffer has
;; free entries, enqueuing is very fast with no waiting.  Once the
;; buffer is full, enqueue must wait until at least one entry is
;; consumed. 
;;
;; The initial version at Ravenpack was loosely based on https://
;; github.com/cameron314/readerwriterqueue/blob/master/readerwriterqueue.h
;;

(defpackage :user (:use :mp))

(defstruct btaqctl
  ;; This struct holds private internal data of the buffered queue.
  ;; Structuer slots may be accessed slightly faster than class slots.
  (head  0)     ;;; Index of oldest entry in buffer.
  (tail 0)      ;;; Index of most recent entry in buffer.
  ;; The following two gates are used as semaphores that keep track
  ;; of the number of items in the corresponding sets.
  ;; This gate is open as long as there are items in the queue buffer.
  ;; The semaphore is incremented only by the provider thread,
  ;; and decremented only by the consumer thread.
  (count-sem  (mp:make-gate nil))
  ;; This gate is open as long as there are empty slots in the buffer.
  ;; The semaphore is incremented only by the consumer thread,
  ;; and decremented only by the provider thread.
  (available-sem (mp:make-gate nil))
  (provider nil)
  (consumer nil)
  (enqueue-wait t)
  (enqueue-whostate nil)
  (data nil)             ;;; The queue buffer.
  )
  

(defclass  buffered-two-actor-queue 
    (mp:queue)
  ((name :reader queue-name :initarg :name :initform nil)
   (btaqctl :initform nil)
   ;; The slot mp::count is inherited from the superclass and used
   ;; by the inherited queue-length method; but it must be maintained 
   ;; atomically by the enqueue and dequeue methods.
   ))

(defmethod initialize-instance :after ((instance buffered-two-actor-queue) 
				       &key (buffer-size 1000)
					    provider consumer
					    (enqueue-wait t e-w-p)
					    enqueue-whostate 
				       &aux (ctl (make-btaqctl
						  :data (make-array  buffer-size)
						  ))
					    (av (btaqctl-available-sem ctl))
					    )
  (with-slots (name btaqctl) instance
    (setf btaqctl ctl)
    (setf 
	  (btaqctl-provider ctl) provider
	  (btaqctl-consumer ctl) consumer
	  (btaqctl-enqueue-whostate ctl)
	  (if enqueue-whostate
	      enqueue-whostate
	    (format nil "Waiting for room in buffered queue~A~A"
		    (if name " " "") (or name ""))))
    (when e-w-p (setf (btaqctl-enqueue-wait ctl) enqueue-wait))
    (dotimes (i buffer-size)
      ;; All the slots in the buffer are available initially.
      (mp:put-semaphore av))))

(defmethod enqueue ((queue buffered-two-actor-queue) item 
		    &aux (me mp:*current-process*) done)
  (with-slots (mp::count btaqctl) queue
    (let* ((ctl btaqctl)
	   (wait (btaqctl-enqueue-wait ctl))
	   (available-sem (btaqctl-available-sem ctl))
	   (whostate (btaqctl-enqueue-whostate ctl))
	   (data (btaqctl-data ctl))
	   (count-sem (btaqctl-count-sem ctl))
	   (size (length data))
	   )
      ;; Allow write access by only one thread.
      (or (eq me (btaqctl-provider ctl))
	  (atomic-conditional-setf (btaqctl-provider ctl) me nil)
	  (error "Not the registered provider of this queue."))
      (loop
	;; This loop will run once if there is room in the buffer,
	;; and a second time if it was necessary to wait for room.
	;; This loop can iterate indefinitely if perverse interupts
	;; grab the available slot as soon as this interation 
	;; discovers one.
	(cond
	 ((with-delayed-interrupts
	    ;; Once we know that there is a slot available, we cannot allow
	    ;; an interrupt while we fill the empty slot and update the queue.
	    ;; 
	    ;; Lisp interrupts must be delayed because the following sequence of
	    ;; steps must be done atomically with respect to this thread.
	    (when (mp:gate-open-p available-sem)
	      (mp:get-semaphore
	       ;; This operation must be globally atomic because the consumer 
	       ;; thread also modifies this object.
	       ;; This call will never wait because this is the only
	       ;; thread that decrements the semaphore.
	       available-sem)
	      (let* ((current (btaqctl-head ctl))
		     (next (rem (1+ current) size)))
		(setf (svref data current) item)
		(setf (btaqctl-head ctl) next) 
		(mp:put-semaphore count-sem)
		t)))
	  ;; The count slot need not be updated inside the above
	  ;;;delayed interrupt context because its value is not used by 
	  ;;any of the internal function of this queue class.
	  (excl:incf-atomic mp::count)
	  (setq done t)
	  (return))
	 ((null wait) (error "~S queue buffer full" queue))
	 ((realp wait) (cond 
			((<= wait 0) (return))
			((mp:process-wait-with-timeout
			  whostate wait #'mp:gate-open-p available-sem)
			 ;; Loop back to try storing again.
			 )
			(t (return))))
	 (t (mp:process-wait whostate #'mp:gate-open-p available-sem))))
      done)))

(defmethod dequeue ((queue buffered-two-actor-queue)
                    &key wait empty-queue-result
			 (whostate "waiting for queue")
		    &aux (me mp:*current-process*) 
			 current next result got-entry)
  (with-slots (mp::count btaqctl) queue
    (let* ((ctl btaqctl)
	   (data (btaqctl-data ctl))
	   (count-sem (btaqctl-count-sem ctl))
	   (size (length data))
	   )
      (or (eq me (btaqctl-consumer ctl))
	  (atomic-conditional-setf (btaqctl-consumer ctl) me nil)
	  (error "Not the registered consumer of this queue."))
      (loop
	;; This loop will run once if there an item in the buffer,
	;; and a second time if it was necessary to wait for one.
	;; (Or many times, see comment in enqueue.)
	(cond
	 ((with-delayed-interrupts
	    ;; Once we know that there is an entry available,
	    ;; we cannot allow an interrupt while we pull out the 
	    ;; available entry and update the queue.
	    (when (mp:gate-open-p count-sem)	  
	      ;; Make sure to always decrement the semaphore.
	      ;; This get will never wait because this is
	      ;; the only thread decrementing this semaphore.
	      (mp:get-semaphore count-sem)
	      (setq current (btaqctl-tail ctl))
	      (setq next (rem (1+ current) size))
	      (setq result (svref data current))
	      ;; flush the item so it can be GC'd
	      (setf (svref data current) nil) 
	      (setf (btaqctl-tail ctl) next)
	      (put-semaphore (btaqctl-available-sem ctl))
	      t))
	  ;; The count slot need not be updated inside the above
	  ;;;delayed interrupt context because its value is not used by 
	  ;;any of the internal function of this queue class.
	  (decf-atomic mp::count)
	  (setq got-entry t)
	  (return))
	 ((null wait) (error "~S queue is empty" queue))
	 ((realp wait)
	  (cond ((<= wait 0) (return))
		((mp:process-wait-with-timeout
		  whostate wait #'mp:gate-open-p count-sem)
		 ;; Loop back to grab the entry.
		 )
		(t (return))))
	 (t (mp:process-wait whostate #'mp:gate-open-p count-sem))))))
  (if got-entry result empty-queue-result))

;; The queue-length method is inherited from the mp:queue superclass.

(defmethod enqueue-wait ((queue buffered-two-actor-queue))
  (with-slots (btaqctl) queue (btaqctl-enqueue-wait btaqctl)))

(defmethod (setf enqueue-wait) (wait (queue buffered-two-actor-queue))
  (with-slots (btaqctl) queue (setf (btaqctl-enqueue-wait btaqctl) wait)))

(defmethod enqueue-whostate ((queue buffered-two-actor-queue))
  (with-slots (btaqctl) queue (btaqctl-enqueue-whostate btaqctl)))

(defmethod (setf enqueue-whostate) (wait (queue buffered-two-actor-queue))
  (with-slots (btaqctl) queue (setf (btaqctl-enqueue-whostate btaqctl) wait)))

(defmethod queue-buffer-size ((queue buffered-two-actor-queue))
  (with-slots (btaqctl) queue (length (btaqctl-data btaqctl))))

(defmethod queue-empty-p ((queue buffered-two-actor-queue))
  (with-slots (btaqctl) queue (not (gate-open-p (btaqctl-count-sem btaqctl)))))

(defmethod queue-full-p ((queue buffered-two-actor-queue))
  (with-slots (btaqctl) queue (not (gate-open-p (btaqctl-available-sem btaqctl)))))

(defmethod queue-entry-p ((queue buffered-two-actor-queue) 
			  &key wait whostate &aux count-sem)
  (with-slots (btaqctl) queue
    (setq count-sem (btaqctl-count-sem btaqctl))
    (cond ((and (or (null wait)
		    (and (realp wait) (<= wait 0)))
		(not (mp:gate-open-p count-sem)))
	   nil)
	  ((realp wait) (if (mp:process-wait-with-timeout
			     whostate wait #'mp:gate-open-p count-sem)
			    t
			  nil))
	  ((mp:process-wait whostate wait #'mp:gate-open-p count-sem)
	   t)
	  (t nil))))



