;; Bidirectional (dual channel) rot13 encapsulator
;;
;; This source code is in the public domain.

(in-package :user)

(eval-when (compile)
  (require :iodefs))

;; ==============
;; Example: rot13 (bidirectional):

(def-stream-class rot13-bidirectional-stream (bidirectional-character-encapsulating-stream)
  ())

(defmethod device-read ((stream rot13-bidirectional-stream) buffer start end blocking)
  (when (and (null buffer) (not (eq start end)))
    (with-stream-class (rot13-bidirectional-stream)
      (setq buffer (sm excl::buffer stream)
	    end (length buffer))))
  (with-stream-class (rot13-bidirectional-stream stream)
    (let ((base-stream (sm excl::input-handle stream)))
      (if (and base-stream (>= end start))
	  (let ((res 0))
	    (when (or blocking (stream-listen base-stream))
	      (unless blocking (setq res -3))
	      (when (> end start)
		(setq res (read-vector buffer base-stream :start start :end end))
		(when (> res 0)
		  (setf (sm excl::buffpos stream) start)
		  (loop for i from start below (+ start res)
		      do (setf (char buffer i)
			   (rotate-char (char buffer i)))))))
	    (setf (sm excl::buffer-ptr stream) res))
	0))))

(defmethod device-write ((stream rot13-bidirectional-stream) buffer start end blocking)
  (let ((flush (eq buffer :flush))
	(res 0))
    (when (and (or flush (null buffer)) (not (eq start end)))
      (with-stream-class (rot13-bidirectional-stream)
	(setq buffer (sm excl::out-buffer stream))))
    (with-stream-class (rot13-bidirectional-stream stream)
      (let ((base-stream (sm excl::output-handle stream)))
	(when (and base-stream (> end start))
	  (setq res (- end start))
	  (loop for i from start upto end
	      do (setf (char buffer i)
		   (rotate-char (char buffer i))))
	  (write-string buffer base-stream :start start :end end)
	  (when flush
	    (if blocking (finish-output base-stream) (force-output base-stream))))))
    res))

(defun rotate-char (char)
  (unless (alpha-char-p char)
    (return-from rotate-char char))
  (let ((code (char-code char)))
    (cond ((<= #.(char-code #\A) code #.(char-code #\Z))
	   (code-char
	    (+ #.(char-code #\A)
	       (mod (+ 13 (- code #.(char-code #\A))) 26))))
	  (t
	   ;; assume ascii; must be lowercase letter
	   (code-char
	    (+ #.(char-code #\a)
	       (mod (+ 13 (- code #.(char-code #\a))) 26)))))))
