`rot13b.cl` contains an example of a bidirectional stream that converts
characters using the rot13 algorithm.  [The current documentation](https://franz.com/support/documentation/current/doc/streams.htm#encap-example-rot13b-2)

To use the example, start an Allegro Common Lisp and compile and load
the source:

    cl-user(1): :cl rot13b
    ;;; Compiling file rot13b.cl
    ; Fast loading from bundle code/iodefs.fasl.
    ;   Fast loading from bundle code/iordefs.fasl.
    ;     Fast loading from bundle code/efmacs.fasl.
    ;;; Writing fasl file rot13b.fasl
    ;;; Fasl write complete
    ; Fast loading
    ;    .../rot13b.fasl
    cl-user(2)

Now make a rot13 stream that encapsulates *terminal-io*:

    cl-user(2): (setq s (make-instance 'rot13-bidirectional-stream :base-stream *terminal-io*))
    #<rot13-bidirectional-stream "..." pos 0 @ #x10000dbdd52>
    cl-user(3):

Now various inputs can be translated back and forth in rot13 style:


    cl-user(3): (read-line s)
    The quick brown fox jumped over the lazy dog.
    "Gur dhvpx oebja sbk whzcrq bire gur ynml qbt."
    nil
    cl-user(4): (read-line s)
    Gur dhvpx oebja sbk whzcrq bire gur ynml qbt.
    "The quick brown fox jumped over the lazy dog."
    nil
    cl-user(5): 

