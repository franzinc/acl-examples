`process-pool.cl` contains an example use of the new process pool
facility, released as a patch after the initial 10.1 release.  [The 10.1 documentation](https://franz.com/support/documentation/10.1/doc/multiprocessing.htm#process-pools-1).

To use the example, start the SMP version of Allegro Common Lisp and
compile and load the example code:

    cl-user(1): :cl process-pool/process-pool.cl 
    ;;; Compiling file process-pool/process-pool.cl
    ; Fast loading
    ;    /net/quadra/acl/10.1/bin/linuxamd64.64smp/code/process-pool.001
    ;;; Installing process-pool patch, version 1.
    ; Fast loading
    ;    /net/quadra/acl/10.1/bin/linuxamd64.64smp/code/smputil.fasl
    ;   Fast loading
    ;      /net/quadra/acl/10.1/bin/linuxamd64.64smp/code/smp-utilities.001
    ;;; Installing smputil patch, version 1.
    ;;; Writing fasl file process-pool/process-pool.fasl
    ;;; Fasl write complete
    ; Fast loading
    ;    /net/gremlin/home/layer/src/acl-examples/process-pool/process-pool.fasl

First, run in one thread to establish a baseline timing:

    cl-user(2): (time (simple-parallel-task :n 1 :in *in* :out *out* :r 50))
    ; cpu time (non-gc) 5.419177 sec user, 0.000000 sec system
    ; cpu time (gc)     0.000000 sec user, 0.000000 sec system
    ; cpu time (total)  5.419177 sec user, 0.000000 sec system
    ; cpu time (thread) 0.001999 sec user, 0.001000 sec system
    ; real time  5.421431 sec (99.96%)
    ; space allocation:
    ;  3,796 cons cells, 65,472 other bytes, 0 static bytes
    ; Page Faults: major: 0 (gc: 0), minor: 75 (gc: 0)
    (50000000)

Then, run in three threads takes about 1/3 of the time:

    cl-user(3): (time (simple-parallel-task :n 3 :in *in* :out *out* :r 50))
    ; cpu time (non-gc) 5.418177 sec user, 0.000000 sec system
    ; cpu time (gc)     0.000000 sec user, 0.000000 sec system
    ; cpu time (total)  5.418177 sec user, 0.000000 sec system
    ; cpu time (thread) 0.001000 sec user, 0.000000 sec system
    ; real time  1.848175 sec (293.2%)
    ; space allocation:
    ;  573 cons cells, 17,040 other bytes, 0 static bytes
    ; Page Faults: major: 0 (gc: 0), minor: 30 (gc: 0)
    (50000000)
    cl-user(4): 
