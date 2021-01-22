(ns camsaul.logic.examples.make-change-test
  (:require [camsaul.logic.examples.make-change :refer :all]
            [clojure.test :refer :all]))

(def us-coins
  {:penny       1
   :nickel      5
   :dime        10
   :quarter     25
   :half-dollar 50
   :dollar      100})

(def british-coins
  {:penny         1
   :two-p         2
   :five-p        5
   :ten-p         10
   :twenty-p      20
   #_:twenty-five-p #_25 ; technically this exists but it's not super common
   :fifty-p       50
   :one-pound     100
   :two-pound     200})

(deftest make-change-test
  (is (= {:num-coins 0}
         (make-change 0)))
  (doseq [[amount coin-set->expected]
          {0.01 {us-coins        {:num-coins 1, :penny 1}
                 british-coins   {:num-coins 1, :penny 1}
                 common-us-coins {:num-coins 1, :penny 1}}
           0.50 {us-coins        {:num-coins 1, :half-dollar 1}
                 british-coins   {:num-coins 1, :fifty-p 1}
                 common-us-coins {:num-coins 2, :quarter 2}}
           0.71 {us-coins        {:num-coins 4, :half-dollar 1, :dime 2, :penny 1}
                 british-coins   {:num-coins 3, :fifty-p 1, :twenty-p 1, :penny 1}
                 common-us-coins {:num-coins 5, :quarter 2, :dime 2, :penny 1}}
           0.99 {us-coins        {:num-coins 8, :half-dollar 1, :quarter 1, :dime 2, :penny 4}
                 british-coins   {:num-coins 6, :fifty-p 1, :twenty-p 2, :five-p 1, :two-p 2}
                 common-us-coins {:num-coins 9, :quarter 3, :dime 2, :penny 4}}}

          [coin-set expected] coin-set->expected]
    (testing (format "make change for %.03f with %s" amount coin-set)
      (binding [*coin-set* coin-set]
        (is (= expected
               (make-change amount)))))))

(deftest prefer-larger-coins-test
  (testing "Given 10, 20, and 30 coins, how do make 40? 30 + 10 or 2x20?? What's the right answer?"
    ;; I'm not 100% sure how the logic engine is determining what to try, so I can't yet
    (let [l [:large 30]
          m [:medium 20]
          s [:small 10]]
      (doseq [coin-set [[l m s]
                        [l s m]
                        [m l s]
                        [m s l]
                        [s l m]
                        [s m l]]
              :let [coin-set (into (array-map) coin-set)]]
        (testing (format "coin-set = %s" (pr-str coin-set))
          (binding [*coin-set* coin-set]
            (is (contains? #{{:num-coins 2, :medium 2}
                             {:num-coins 2, :large 1, :small 1}}
                           (make-change 0.4)))))))))

(deftest round-fractional-amount-test
  (is (= (make-change 0.99)
         (make-change 0.995)))
  (testing "override the fractional-amount-fn - round instead of keeping fractional amounts"
    (binding [*round-fractional-amount-fn* #(long (Math/round %))]
      (doseq [[amount rounded-amount] {0.995 1.0
                                       0.994 0.99}]
        (testing amount
          (is rounded-amount
              (*round-fractional-amount-fn* amount))
          (is (= (make-change rounded-amount)
                 (make-change amount))))))))

(deftest quantities-test
  (doseq [[quantities expected] {{:quarter 2, :dime 100}  {:num-coins 7, :quarter 2, :dime 5}
                                 {:quarter 1, :dime 100}  {:num-coins 10, :dime 10}
                                 {:nickel 18, :penny 100} {:num-coins 28, :nickel 18, :penny 10}
                                 ;; not possible
                                 {:quarter 2}             nil}]
    (testing (format "quantities = %s" (pr-str quantities))
      (binding [*quantities* quantities]
        (is (= expected
               (make-change 1.0)))))))
