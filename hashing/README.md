
11.0 and later only:

This directory provides two examples of def-hash-table-implementation
usage:

   alisthash.cl:  provides an incomplete implementation of an
   alist-based hash-table (remhash is explicitly left out as an
   exercise for the reader).  It is not smp-safe, but provides basic
   concepts of hash-table implementation generation.

   Quickhash.cl is based on the hash-table-like implementation called
   quicktab.cl in the agraph repository, which features extremely fast
   hash generation and rehashing.  This code performs with similar
   timing and consing characteristics.  It is not smp thread-safe.
