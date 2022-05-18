# Implementation of a Restricted Queue Subclass
## Files in this directory

* `buffered-queue.cl` The definition of class buffered-two-actor-queue
   and its methods.

* `test-bq.cl` Functions that test the implementation and some
examples of how the performance could be measured.

## Class buffered-two-actor-queue         

This subclass of mp:queue is restricted to a single provider and a
single consumer thread, and holds entries in a fixed size buffer.
These limitatons allow a significant improvement in performance
in some cases.

### Initargs:

* `:buffer-size` - the maximum number of entries that can be stored in
the queue.  When this buffer is full, the behavior is controlled by
the :enqueue-wait initarg.

* `:enqueue-wait` specifies the behavior when an `enqueue` operation must wait.
  * `t`   - (the default) Wait until space is available and then add
the new entry.
  * `0`     - If the buffer is full, return `nil` immediately.
  * `n>0`   - Wait at most `n` seconds for space, return `nil` on timeout.
  * `nil`   - Signal an error.
  * *other* - Same as `t`.

* `:enqueue-whostate` - The process whostate while waiting for free
space.

* `:provider` - If specified, it must be a Lisp process instance ; it
will be the only thread allowed to call `enqueue`.  If not specified,
the first thread to call `enqueue` will become the provider.

* `:consumer` - If specified, it must be a Lisp process instance ; it
will be the only thread allowed to call `dequeue`.  If not specified,
the first thread to call `dequeue` will become the consumer.

### Accessors:

`enqueue-wait` Query the action to be taken if `enqueue` is called when the queue buffer is full.
Can be used with `setf` to modify the action.  Valid values are the ones allowed for the `:enqueue-wait`
initarg.

`enqueue-whostate` Query the whostate used if `enqueue` must wait.
Can be used with `setf` to modify the value.

### Generic Functions

`enqueue` *queue* *item*                

The method for `buffered-two-actor-queue`adds *item* to the queue if there
is room in the buffer and returns `t`; if room is not immediately available
wait until space becomes available or until some time limit is reached;
if *item* is added, return `t`, otherwise return `nil`.
The wait duration is controlled by the `enqueue-wait`setting.
If the setting is `t` (the default), wait indefinitely.
If the setting is `nil, signal an error immediately.
If the setting is `0` or a negative number return immediately.
If the setting is a positive number, wait at most that many seconds.
If any other value, wait indefinitely.
While waiting, use the `enqueue-whostate` setting as the wait whostate.


`queue-full-p` *queue*

The method for `buffered-two-actor-queue` returns `t` if calling enqueue
would require waiting for the consumer the remove at least one entry.
This function may be called from any thread; in the provider thread
a `nil` result can change only if the thread calls enqueue;
in the consumer thread, a `t` result can change only if the thread
calls `dequeue`; in any
other thread the result can become obsolete at any moment.

`queue-entry-p` *queue* `&key wait whostate`       

The method for buffered-two-actor-queue returns `t` when there is an
entry in the queue. The keyword arguments control waiting behavior
in the same way as in `enqueue`.
This function may be called from any thread; in the consumer thread
a true result can change only if the thread calls `dequeue`; in
the provider thread, a false result can chage only if the thread
calls `enqueue`.
In any other thread the result can become obsolete at any moment.


