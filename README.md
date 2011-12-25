# Mike's Clojure Utilities

This is a catchall repo for Clojure utilities I've been using for various projects. Hey, maybe you can use them too.

## Underflow

Underflow is a function trampoline that supports reifiable program states.

Highly functional code in Clojure has two major limitations:
first, no tail recursion means that certain functional idioms,
when implemented in Clojure, will blow the stack.
Continuation-passing style is one such idiom.

Second, Clojure doesn't give you the ability to capture the state
of global vars. This is also a limitation of every Lisp I know of.
However, given Clojure's emphasis on concurrent and/or lazy code,
this absence is a little glaring; it means that you can't
ever use vars with a computation that doesn't fit in the single-thread model.

## License

Copyright (C) 2011 Mike Thvedt

This version of Mike's Clojure Utilities is distributed under the Beerware
Public License. So long as you retain this notice in this file under the heading
'License', you may do whatever you want with this stuff. If you find this stuff
useful, you may buy me a beer sometime, or drink a beer in my honor.
