(ns camsaul.logic.examples.make-change-2-test
  (:require [camsaul.logic.examples.make-change-2 :refer :all]
            [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [clojure.test :refer :all]))

(deftest sortedo-test
  (is (= [[5 5 5 10 25]]
         (l/run 5 [sorted] (sortedo sorted [5 10 5 5 25]))))
  (testing "nums is an lvar"
    (is (= [[5 5 5 10 25]]
           (l/run 5 [sorted]
             (l/fresh [nums]
               (l/== nums [5 10 5 5 25])
               (sortedo sorted nums)))))))

(deftest change-using-num-coins-o-test
  (let [coin-set  [1 5 10 25]
        amount    45
        num-coins 5]
    (is (= [[5 5 5 5 25]
            [5 10 10 10 10]]
           (l/run 4 [coins]
             (change-using-num-coins-o coins amount coin-set num-coins))))))

(deftest changeo-test
  (testing "WITH min-coins specified"
    (is (= [[5 5 5 5 25]
            [5 10 10 10 10]]
           (l/run 2 [coins]
             (changeo coins 45 [1 5 10 25] 5)))))
  (testing "without min-coins specified"
    (is (= [[5 5 5 5 25]
            [5 10 10 10 10]]
           (l/run 20 [coins]
             (changeo coins 45 [1 5 10 25])
             (counto coins 5))))))

(deftest make-change-test
  (is (= [1 5]
         (make-change 6 [1 5 10 25])))
  (is (= [3 3]
         (make-change 6 [3 4])))
  (is (= [3 3]
         (make-change 6 [1 3 4])))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"No possible ways to make change"
       (make-change 6 [5 7]))))

(deftest test-2
  (testing "solve for coins"
    (let [coin-set [1 5 10 25]
          amount   45]
      (is (= [[10 10 25]
              [5 5 10 25]
              [5 5 5 5 25]
              [5 10 10 10 10]]
             (l/run 4 [coins]
               (changeo coins amount coin-set))))))

  (testing "solve for amount"
    (let [coin-set [1 5 10 25]
          coins    [10 10 25]]
      (is (= [45]
             (l/run 1 [amount]
               (changeo coins amount coin-set))))))

  (testing "solve for coin set"
    (let [coins  [10 10 25]
          amount 45]
      (is (= [[10 25]
              [10 25 26]]
             (l/run 2 [coin-set]
               (changeo coins amount coin-set))))))

  (testing "solve for coin set with restrictions"
    (let [coins  [10 10 25]
          amount 45]
      (is (= [[10 25]
              [10 25 30]]
             (l/run 2 [coin-set]
               (changeo coins amount coin-set)
               ;; coins have to be a multiple of 5
               (everyg* (fn [coin]
                          (fd/in coin (apply fd/domain (range 5 100 5))))
                        coin-set))))))

  (testing "solve for coin set AND amount"
    (let [coins [10 10 25]]
      (is (= [[[10 25] 45]]
             (l/run 1 [coin-set amount]
               (changeo coins amount coin-set))))))

  (testing "solve for coins AND amount"
    (let [coin-set [1 5 10 25]]
      (is (= [[[]    0]
              [[1]   1]
              [[5]   5]
              [[10] 10]
              [[25] 25]]
             (l/run 5 [coins amount]
               (changeo coins amount coin-set))))))

  (testing "solve for coins AND amount with restrictions"
    (let [coin-set [1 5 10 25]]
      (is (= [[[25]     25]
              [[1 25]   26]
              [[5 25]   30]
              [[10 10]  20]
              [[1 1 25] 27]]
             (l/run 5 [coins amount]
               (changeo coins amount coin-set)
               (fd/>= amount 20)
               (fd/<= amount 30))))))

  (testing "solve for coin set and coins with restrictions"
    (let [amount 60]
      (l/run 5 [coin-set coins]
        (counto coin-set 2)
        (counto coins 3)
        (l/everyg
         (fn [coin]
           (l/pred coin odd?))
         coin-set)
        (changeo coins amount coin-set)))))
