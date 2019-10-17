This example implements a `telnet` server.  It's useful for server
applications, when something goes wrong, to be able to `telnet` into
the server and poke around.  Here, we just compile, load and start the
server:

    cl-user(2): :cl telnet-server.cl
    ;;; Compiling file telnet-server.cl
    ;;; Writing fasl file telnet-server.fasl
    ;;; Fasl write complete
    ; Fast loading /net/gremlin/home/layer/src/acl-examples/telnet-server.fasl
    cl-user(3): (start-telnet-server)
    #<multiprocessing:process telnet server(7) @ #x209d0732>
    cl-user(4): 
    ; Autoloading for acl-socket:configure-dns:
    ; Fast loading from bundle code/acldns.fasl.
    telnet server: new connection from localhost
    telnet server: closing connection from localhost

Now, in a terminal/BASH:

    $ telnet localhost 9999
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.

    WARNING: do not use :exit or (exit).  Use (quit) to quit.
    cl-user(1): :zoom
    :zoom
    Evaluation stack:
    	       
     ->(tpl:top-level-read-eval-print-loop)
       (tpl:start-interactive-top-level
          #<multivalent stream socket connected from localhost/9999 to
            localhost/59100 @ #x209d126a>
          tpl:top-level-read-eval-print-loop ...)
       (start-telnet-session
          #<multivalent stream socket connected from localhost/9999 to
            localhost/59100 @ #x209d126a>
          "localhost")
    cl-user(2): :proc
    :proc
    DP Seq Dis Sec   dSec Pri State    Process Name, Whostate, Arrest
     *   2   4   0    0.3   0 waiting  Initial Lisp Listener, waiting-for-input
     *   7   3   0    0.0   0 waiting  telnet server, waiting for input
     *   9   3   0    0.0   0 runnable telnet session
     *   3   5   0    0.0   0 waiting  Connect to Emacs daemon, waiting for input
     *   6   2   0    0.0   0 inactive Run Bar Process
     *   8   1   0    0.0   0 waiting  Domain Name Server Client, waiting for input
     *   5   2   0    0.0   0 waiting  Editor Server, waiting-for-input
    cl-user(3): (exit)
    (exit)
    Use (quit) instead of exit.
    nil
    cl-user(4): (quit)
    (quit)
    Connection closed by foreign host.
    $ 

`:exit` and `excl:exit` are wrapped so that the server cannot easily
be killed.
