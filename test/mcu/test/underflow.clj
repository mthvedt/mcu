(ns mcu.test.underflow
  (:use clojure.test mcu.underflow))

(=defn test1 [x] x)

(deftest test-defs
         (is (= (test1 1) 1))
         (is (= (=test1 1) 1)))

(=defn consecutive-sum [x x2]
       (if (= x 0)
         x2
         (=tailcall consecutive-sum (dec x) (+ x x2))))

(declare funny-odd?)

(=defn funny-even? [x]
       (if (= x 0)
         true 
         (=tailcall funny-odd? (dec x))))

(=defn funny-odd? [x]
       (if (= x 0)
         false
         (=tailcall funny-even? (dec x))))

(deftest test-continuation
         (is (= 55 (=consecutive-sum 10 0)))
         (is (= 50005000 (=consecutive-sum 10000 0)))
         (is (=funny-even? 1000000))
         (is (not (=funny-even? 1000001)))
         (is (=funny-odd? 10000001))
         (is (not (=funny-odd? 100000))))
