`base64.cl` contains an example of a bidirectional stream that converts
data using the base64 part of MIME encoding.  [The current documentation](https://franz.com/support/documentation/current/doc/streams.htm#encap-example-base64-2)

To use the example, start an Allegro Common Lisp and note the
following file exists with the following contents:

    cl-user(1): (shell "cat country.txt")
    Now is the time
    for all good people
    to come to the aid
    of their country.
    0
    cl-user(2): :cl base64
    ;;; Compiling file base64.cl
    ; Fast loading from bundle code/iodefs.fasl.
    ;   Fast loading from bundle code/iordefs.fasl.
    ;     Fast loading from bundle code/efmacs.fasl.
    ;;; Writing fasl file base64.fasl
    ;;; Fasl write complete
    ; Fast loading
    ;    .../base64.fasl
    cl-user(3):

Now make a mime-encoded file.  This example provides a lisp version of
the base64 encoding.  An older unix program called mpack used to be
available, but it does not exist for some systems:

    cl-user(3): (mpack "country.mime" "country.txt")
    nil
    cl-user(4): (shell "cat country.mime")
    Content-Transfer-Encoding: base64
    
    Tm93IGlzIHRoZSB0aW1lCmZvciBhbGwgZ29vZCBwZW9wbGUKdG8g
    Y29tZSB0byB0aGUgYWlkCm9mIHRoZWlyIGNvdW50cnkuCg==
    0
    cl-user(5):

Now, open the mime-encoded file, and set up a base64 encapsulating
stream on it:

    cl-user(5): (setq yyy (open "country.mime"))
    #<file-simple-stream #P"country.mime" for input pos 0 @ #x10000e63f72>
    cl-user(6): (setq xxx (make-instance 'base64-reader-stream :base-stream yyy))
    #<base64-reader-stream
       for input fd #<file-simple-stream #P"country.mime" for input pos 0>
      @ #x10000e67f02>
    cl-user(7):

The stream is now ready for reads:

    cl-user(7): (read-line xxx)
    "Now is the time"
    nil
    cl-user(8): (read-line xxx)
    "for all good people"
    nil
    cl-user(9): (read-line xxx)
    "to come to the aid"
    nil
    cl-user(10): (read-line xxx)
    "of their country."
    nil
    cl-user(11): (read-line xxx)
    Error: eof encountered on stream
           #<base64-reader-stream
              for input fd #<file-simple-stream
                             #P"country.mime" for input pos 137>
             @ #x10000c7c6b2>
      [condition type: end-of-file]
    
    Restart actions (select using :continue):
     0: Return to Top Level (an "abort" restart).
     1: Abort entirely from this (lisp) process.
    [1] cl-user(12):

Note that file-position on the encapsulated stream will return nil
(because it is a dual-channel stream, whose default
device-file-position method always returns nil).  It would be possible
to extend this example to respond to file-position and also to set the
file-position, but the file-position calculation would be very complex
and the only useful value to set the file-position to would be 0,
unless starting file-positions were recorded as each chunk is read in
(or unless multiple reads were acceptable in determining the
file-position to set).
