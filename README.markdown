Introduction
============

This is clj-iter, an iteration macro for Clojure inspired by the
excellent [iterate] [1] library for Common Lisp (henceforth Iterate).

clj-iter was written by Daniel Janus <dj@danieljanus.pl>.  Please
send any patches or suggestions to that address.  

I wrote clj-iter because I needed a handy way of iterating over
multiple sequences at once.

I did not intend clj-iter to be 100% compatible with Common Lisp
iterate.  Instead, the design goal was to keep it as simple as
possible, and make it blend well with the rest of Clojure.  In
contrast to [cl-loop] [2] (a similar library inspired by Common Lisp's
standard LOOP macro), which uses mutable bindings, clj-iter has a
functional flavour, and macroexpands to the kind of code you would
write manually using loop/recur.

Examples
========

The following code takes a list of integers and sums them with
consecutive natural numbers:

    (iter (for x in [31 41 59 26])
          (for y from 1)
          (collect (+ x y)))
    ==> (32 43 62 30)
    
And here is the macroexpansion:

    (loop [collected4234 nil G__4233 [31 41 59 26] y 1]
      (let [x (first G__4233)]
        (if (and (empty? G__4233))
          (reverse collected4234)
          (do
           (recur
            (if true (cons (+ x y) collected4234) collected4234)
            (next G__4233)
            (inc y))))))

The following function takes a number (size) and a collection, 
and transforms each element into a list containing itself and
neighbouring elements, i.e., the distance of which from the
element in question is not greater than size:

    (defn sliding-window [coll size]
      (iter (for s on coll)
            (for q initially () then (cons (first s) q))
            (collect (cons (first s) (concat (take size (rest s)) (take size q))))))

This is a neat (and extremely readable) example of raising to a
positive integer power (yes, I know there's clojure.contrib.math,
but just for the sake of example...):

    (defn pow [x y]
      (iter (repeat y) (multiply x)))

Differences from Iterate
========================

 * clj-iter supports only the following clauses:

        FOR var FROM starting-value [TO/BELOW ending-value] [BY step-value]
        FOR var DOWNFROM starting-value [TO/BELOW ending-value] [BY step-value]
        FOR var IN sequence
        FOR var ON sequence
        FOR var INITIALLY initial-expr THEN then-expr [STOP stop-condition]
        FOR var = expr
        REPEAT number-of-times
        RETURN expr IF condition
        COLLECT expr [IF condition]
        SUM expr [IF condition]
        MULTIPLY expr [IF condition]

   The symbols that are treated specially by clj-iter are spelled
   here in uppercase to distinguish them visually, but they should
   appear in lowercase in the source code.  Just like in Iterate,
   they can also be keywords (i.e., you can equivalently say
   (for i from 1) or (:for i :from 1)).  

   The optional subclauses are shown here in square brackets.

   Patches to support other Iterate functionality are most welcome!

 * There is currently no proper documentation except this file, but
   the unit tests at the end of iter.clj should give you some more
   examples of how to use clj-iter.  

 * Unlike Iterate, clj-iter does not do code-walking.  The entire body
   of the ITER macro should consist of either clj-iter clauses, or
   forms to be evaluated (and results discarded) on each iteration;
   these forms cannot contain clj-iter clauses.  This means that the
   COLLECT clause has an optional conditional IF subclause; e.g., to
   collect all the even numbers of a series, you should write

        (iter (for x from 0 to 9)
              (collect x if (even? x)))

   instead of Common Lisp

        (iter (for x from 0 to 9)
              (when (evenp x) 
               (collect x)))

 * Unlike Iterate, clj-iter uses the same prepositions (IN and ON) for
   iterating over all seqable data, regardless of whether they are
   lists, vectors or maps.

 * Destructuring works in FOR clauses:
   
        (iter (for [k v] in {:a 2, :b 3, :c 4})
              (collect k if (even? v)))
        ==> (:a :c)

 * In clj-iter, all bindings are established simultaneously on each
   iteration.  Thus, the following returs first Fibonacci numbers:

        (iter (for a from 0 to 10)
              (for x initially 0 then y)
              (for y initially 1 then (+ x y))
              (collect x))
        ==> (0 1 1 2 3 5 8 13 21 34 55)
        
   In Iterate, it would return (0 1 2 4 8 16 32 64 128 256 512).

   [1]: http://common-lisp.net/project/iterate/
   [2]: http://github.com/tayssir/cl-loop/