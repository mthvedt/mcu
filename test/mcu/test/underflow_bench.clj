(ns mcu.test.underflow-bench
  (:use mcu.underflow mcu.test.underflow))

#_(todo def bench as though its a test, def run-benchmarks macro)

(def
  ^{:doc "When this is not false,
         runs benchmarks defined with the bench macro."}
  *load-benchmarks* true)

(defn- printtime [time-in-nanos]
  (print "Elapsed time per run: ")
  (let [dbltime (double time-in-nanos)]
    (cond
      (< dbltime 10000) (println dbltime "ns")
      (< dbltime (* 10000 1000)) (println (/ dbltime 1000.0) "microsecs")
      (< dbltime (* 10000 1000 1000)) (println (/ dbltime 1000000.0 "ms"))
      true (println (/ dbltime 1000000000.0 "s")))))

(defmacro time2 [divisor expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (printtime (/ (- (. System (nanoTime)) start#)
                   (double ~divisor)))))

(defmacro defbench [thename times microexpr]
  "A better microbenchmark macro that runs gc, finalizers,
  and warms up the JIT compiler.
  The microbenchmark may be run thousands of times to warm up the JIT compiler,
  so make sure it is legitly micro.

  If *run-benchmarks* is not false,
  bench will prepare the JVM as described above
  then time how long it takes to run the given expr the given # of times,
  printing results to System/out."
  `(if *load-benchmarks*
     (do
       (println "Warming up:" ~thename)
       (dotimes [i# 10000] ~microexpr)
       (System/gc)
       (System/runFinalization)
       (println "Benchmarking:" ~thename)
       (time2 ~times (dotimes [i# ~times] ~microexpr)))))

(defn- recursive-consecutive-sum [x x2]
  (if (= x 0)
    x2
    (recursive-consecutive-sum (dec x) (+ x x2))))

(defn- fast-consecutive-sum [xobj]
  (let [minusone (int -1)
        zero (int 0)
        x (int xobj)]
    (loop [i x
           thesum zero]
      (if (= i zero)
        thesum
        (recur (unchecked-add i minusone) (unchecked-add i thesum))))))

(defbench "Recursive consecutive sum" 10000 (recursive-consecutive-sum 1000 0))
(defbench "Fast consecutive sum" 10000 (fast-consecutive-sum 1000))
(defbench "Underflow consecutive sum" 10000 (=consecutive-sum 1000 0))
