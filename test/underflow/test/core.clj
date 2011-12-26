(ns underflow.test.core
  (:use clojure.test underflow.core))

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

(deftest test-tailcall
         (is (= 55 (=consecutive-sum 10 0)))
         (is (= 50005000 (=consecutive-sum 10000 0)))
         (is (=funny-even? 1000000))
         (is (not (=funny-even? 1000001)))
         (is (=funny-odd? 10000001))
         (is (not (=funny-odd? 100000))))

(deftest test-let
         (is (= 6 (underflow #(=let [z (+ 1 2)] (+ z 3)))))
         (is (= 9 (underflow #(=let [z (+ 1 2) z2 (+ z 3)] (+ z z2)))))
         (is (= 9 (underflow #(=let [z (+ 1 2)]
                                    (=let [z2 (+ z 3)]
                                          (+ z z2)))))))

(def mytree [[[1 2] 3] [[4 5] [6 7]]])

(def *tree-stack*)
(def *rval*)

(declare restart)

(defn dft-node [tree]
  (cond (nil? tree) (=tailcall restart)
        (and (coll? tree) (seq tree))
        (=bind-cc cc
                  (set! *tree-stack*
                        (cons #(=let [cval (=tailcall
                                             dft-node
                                             (second tree))]
                                     (=continue cc cval))
                              *tree-stack*))
                  (=tailcall dft-node (first tree)))
        true tree))

(defn restart []
  (if (empty? *tree-stack*)
    ::done
    (let [cont (first *tree-stack*)]
      (set! *tree-stack* (rest *tree-stack*))
      (=tailcall cont))))

(defn dft2 [tree]
  (=let [node (=tailcall dft-node tree)]
        (if (= ::done node)
          *rval*
          (do (set! *rval* (conj *rval* node))
            (=tailcall restart)))))

(defn =dft2 [tree]
  (binding [*tree-stack* []
            *rval* []]
    (underflow dft2 tree)
    *rval*))

(deftest test-dft-continuations
         (is (= [1 2 3 4 5 6 7] (=dft2 mytree))))

(defn dftx [tree1 tree2]
  (=let [node1 (=tailcall dft-node tree1)]
        (if (= ::done node1)
          ::done
          (=let [node2 (=tailcall dft-node tree2)]
                (do (set! *rval* (conj *rval* [node1 node2]))
                  (=tailcall restart))))))

(defn =dftx [tree1 tree2]
  (binding [*tree-stack* []
            *rval* []]
    (underflow dftx tree1 tree2)
    *rval*))

(deftest test-more-dft-continuations
         (is (= (=dftx [[1 2] 3] [4 [5 6]])
                [[1 4] [1 5] [1 6]
                 [2 4] [2 5] [2 6]
                 [3 4] [3 5] [3 6]])))
