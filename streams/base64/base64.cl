;; base64 decoder (read direction only)
;;
;; This source code is in the public domain.

(in-package :user)

(eval-when (compile)
  (require :iodefs))


;; RFC1521 says that 76 characters is the max.  We'll give
;; just a little slop
(eval-when (compile load eval)
  (defconstant base64-buffer-size 80))
  

(def-stream-class base64-reader-stream (dual-channel-simple-stream)
  ((raw-data :initform nil :accessor base64-raw-data)
   (raw-count :initform 0 :accessor base64-raw-count)
   (primed :initform nil :accessor base64-primed)
   (prime-count :initform 0 :accessor base64-prime-count)))

(defmethod device-open ((stream base64-reader-stream) slot-names initargs)
  (declare (ignore slot-names))
  ;; be sure to make an raw data buffer (a string) and its count
  (let* ((base-stream (getf initargs :base-stream)))
    (if base-stream
	(with-stream-class (base64-reader-stream stream)
	  (unless (sm excl::buffer stream)
	    (setf (sm excl::buffer stream)
	      (make-array (device-buffer-length stream) :element-type '(unsigned-byte 8))))
	  (setf (sm excl::input-handle stream) base-stream)
	  (setf (base64-raw-data stream)
	    (make-string (device-buffer-length stream)))
	  (setf (base64-raw-count stream) 0
		(base64-prime-count stream) 0
		(base64-primed stream) nil)
	  (add-stream-instance-flags stream :dual :input :simple)
	  (setf (stream-external-format stream) (sm external-format stream))
	  t)
      (progn (error "base64-reader-stream needs a :base-stream")
	     nil))))

(defmethod device-buffer-length ((stream base64-reader-stream))
  #.base64-buffer-size)

(defvar *base64-decode*
    ;; array to decode base64 data.
    ;; Given an character this will return the 6-bits that character
    ;; contributes.
    ;; The = character is special -- it stands for 0 but means that you've
    ;; hit the end of the data.
    ;;
    ;; 3 bytes (24 bits) of data is encoded as 4 characters, each representing
    ;; 6 bits.
    ;;
    ;; If the data size mod 3 is 0 then there will be no padding.
    ;; if the data size mod 3 is 1 then there will be 2 ='s of padding
    ;; if the data size mod 3 is 2 then there will be 1 =   of padding.
    ;;
    ;; see rfc1521 for details on this
    (let ((res (make-array 256)))
      (do ((i (char-int #\A) (1+ i))
	   (index 0 (1+ index)))
	  ((> i (char-int #\Z)))
	(setf (svref res i) index))
      (do ((i (char-int #\a) (1+ i))
	   (index 26 (1+ index)))
	  ((> i (char-int #\z)))
	(setf (svref res i) index))
      (do ((i (char-int #\0) (1+ i))
	   (index 52 (1+ index)))
	  ((> i (char-int #\9)))
	(setf (svref res i) index))
      (setf (svref res (char-int #\+)) 62)
      (setf (svref res (char-int #\/)) 63)
      res))

(defmethod device-read ((stream base64-reader-stream) buffer start end blocking)
  (let* ((cur start)
	 (decoder *base64-decode*)
	 (res (get-some-raw-data stream blocking (and end (- end start)))))
    (when (<= res 0)
      (return-from device-read res))
    (with-stream-class (base64-reader-stream)
      (when (and (null buffer) (not (eq start end)))
	(setq buffer (sm excl::buffer stream)))
      (let ((raw-buffer (base64-raw-data stream))
	    (raw-count (base64-raw-count stream)))
	(macrolet ((decode-b64-char (ch)
		     `(let* ((xch ,ch)
			     (val (svref decoder (char-int xch))))
			(if* (null val)
			   then (warn "bogus base64 char ~s" xch)
				0
			   else val))))
	  (do ((raw-ix 4 (+ raw-ix 4))
	       ch1 ch2 ch3 ch4 one-byte two-byte)
	      ((> raw-ix raw-count)
	       ;; Copy the non-multiples of 4 to the start of raw-buffer 
	       (decf raw-ix 4)
	       (do ((i 0 (1+ i))
		    (ix raw-ix (1+ ix)))
		   ((eq ix raw-count)
		    (setf (base64-raw-count stream) i))
		 (setf (schar raw-buffer i)
		   (schar raw-buffer ix))))
	    (setq ch1 (schar raw-buffer (- raw-ix 4))
		  ch2 (schar raw-buffer (- raw-ix 3))
		  ch3 (schar raw-buffer (- raw-ix 2))
		  ch4 (schar raw-buffer (- raw-ix 1)))
	    
	    ;; look for termination
	    (cond ((eq ch3 #\=)
		   (setq one-byte t)
		   (setq ch3 #\A  ch4 #\A))
		  ((eq ch4 #\=)
		   (setq two-byte t)
		   (setq ch4 #\A)))
	    
	    (let ((val1 (decode-b64-char ch1))
		  (val2 (decode-b64-char ch2))
		  (val3 (decode-b64-char ch3))
		  (val4 (decode-b64-char ch4)))
	      ;; decode at least one byte
	      (setf (aref buffer cur)
		(+ (ash val1 2)
		   (ash val2 -4)))
	      (incf cur)
	      (unless one-byte
		(setf (aref buffer cur)
		  (logand #xff
			  (+ (ash val2 4)
			     (ash val3 -2))))
		(incf cur)
		(unless two-byte
		  (setf (aref buffer cur)
		    (logand #xff (+ (ash val3 6)
				    val4)))
		  (incf cur))))))
	;; Cur has now been advanced as far past start as we can go.
	(- cur start)))))
      
(defun get-some-raw-data (stream blocking count)
  (declare (optimize speed)) ;; For the dynamic-extent declaration
  (with-stream-class (base64-reader-stream stream)
    (let ((base-stream (sm excl::input-handle stream)) ;; must be a stream
	  (buffer (make-array #.base64-buffer-size :element-type 'character))
	  (raw-buffer (base64-raw-data stream))
	  (raw-count (base64-raw-count stream)))
      (declare (dynamic-extent buffer)
	       (fixnum raw-count))
      (loop
	(with-stream-class (stream base-stream)
	  (multiple-value-bind (res done)
	      (funcall-stm-handler excl::j-read-chars base-stream buffer
				   #\newline 0 (or (and count
							(min count #.base64-buffer-size))
						   #.base64-buffer-size)
				   (not (null blocking)))
	    (when (zerop res)
	      (if* (eq done 't)
		 then (when (eq (base64-primed stream) 'primed)
			(setf (base64-primed stream) 'ready)) ;; blank line after prime string
		 else (return-from get-some-raw-data
			(if (eq done :eof) -1 0))))
	    (unless (base64-primed stream)
	      (setf (base64-primed stream) (prime-it stream buffer res)))
	    (when (and (not (zerop res))
		       (eq (base64-primed stream) 'ready))
	      ;; Currently processing a base64 line.
	      (let ((first-char (schar buffer 0)))
		(if* (member first-char '(#\space #\-))
		   then ;; base64 lines done
			(setf (base64-primed stream) nil
			      (base64-prime-count stream) 0)
		   else ;; Valid base64 line.
			(dotimes (i res)
			  (setf (schar raw-buffer raw-count)
			    (schar buffer i))
			  (incf raw-count))
			(setf (base64-raw-count stream) raw-count)
			(return-from get-some-raw-data res))))))))))



(defun prime-it (stream buffer end)
  ;; Figure out whether we are ready for base64 input yet.
  (let* ((prime-string #1="Content-Transfer-Encoding: base64")
	 (len #.(length #1#))
	 (prime-count (base64-prime-count stream)))
    (dotimes (i end)
      (unless (eql (schar buffer i) (schar prime-string prime-count))
	;; mismatch - start over
	(setf (base64-prime-count stream) 0)
	(return-from prime-it nil))
      (incf prime-count)
      (when (>= prime-count len)
	;; found it.
	(return-from prime-it 'primed)))
    (setf (base64-prime-count stream) prime-count)
    nil))

(defun mpack (tofile fromfile)
  (let* ((string (file-contents fromfile))
	 (base64 (string-to-base64-string string)))
    (setf (file-contents tofile) 
      (format nil "Content-Transfer-Encoding: base64~%~%~a~%" base64))
    nil))
