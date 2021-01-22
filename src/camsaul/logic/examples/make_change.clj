(ns camsaul.logic.examples.make-change
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]))

(def common-us-coins
  {:penny   1
   :nickel  5
   :dime    10
   :quarter 25})

(def ^:dynamic *coin-set* common-us-coins)

(def ^:dynamic *quantities*
  "Map of coin name -> number of coins available for making change. Set this to non-nil to add additional constraints to
  the logic engine. Coins that are not specified are treated as if they have a quantity of zero. e.g.
    (binding [*quantities* {:quarter 2, :dime 100}]
      (make-change 1.0))
    ;; -> {:num-coins 7, :quarter 2, :dime 5}"
  nil)

(def ^:dynamic ^{:arglists '([amount])} *round-fractional-amount-fn*
  "Function used to round fractional change amounts (in cents or equivalent) to an integer e.g. if you're buying gas for
  $3.499 a gallon. You're not getting back tenths of a cent; if you pay $4.00 do you get back $0.50 or $0.51? Default
  implementation just keeps the fractional amount no matter what it is, just like gas stations."
  #(long (Math/floor %)))

(defn possible-total-num-coins-interval
  "Return an `[min max]` interval (inclusive) of all possible numbers of coins that might possibly be able to add up to
  `amount`."
  [coin-set amount]
  ;; 1. You cannot possibly have less than ceil(amount ÷ largest-coin-value) coins. e.g. if I'm making change for
  ;;    $0.95 using the default common coin set I cannot possibly have less than 4 coins in the solution ceil(95 ÷ 25)
  ;;    => 4 3 coins isn't enough to add up to amount -- it can add up to at most $0.75 -- so the best solution has to
  ;;    have *at least* 4 coins.
  ;;
  ;; 2. You cannot possibly have more than amount ÷ smallest-coin-value coins in the solution, e.g. for $0.95 they
  ;;    worst possible solution is 95 pennies and it would be impossible to have more coins than that
  (let [[smallest-coin largest-coin]  (apply (juxt min max) (vals coin-set))]
    (mapv #(long (Math/ceil (/ (double amount) %)))
          [largest-coin smallest-coin])))

(defn sum°
  "A relation such that `summ` is the sum of all numbers in list `l`."
  [l summ]
  (matcha [l]
    ;; sum of empty list => 0
    ([[]]
     (== summ 0))

    ;; sum of list with one element => value of element
    ([[?x]]
     (== summ ?x))

    ;; sum of (a b c...) => value of a + sum of (b c...)
    ([[?head . ?more]]
     (fresh [sum-more]
       (fd/+ ?head sum-more summ)
       (sum° ?more sum-more)))))

(defn make-change*
  "Return a combination of coins that adds up to `amount`."
  [amount]
  ;; create lvars that will track the quantity `n` of each coin and the `total` value of all coins of that type.
  (let [coin-set  *coin-set*
        coins     (vec (for [[coin-name value] coin-set
                             ;; quantity = the max quantity of this coin available, if we're specifying *quantities*
                             :let              [quantity (when *quantities*
                                                           (get *quantities* coin-name 0))]
                             ;; we can go ahead and skip any coins that are too big to make amount
                             :when             (<= value amount)]
                         {:name  coin-name
                          :value value
                          :n     (lvar)
                          :total (lvar)
                          ;; `max-n` = the maximum quantity of this coin, which is the min of *quantities* (if
                          ;; specified) and `amount` ÷ `value` (e.g. can't have more than 3 quarters to make $0.80)
                          :max-n ((fnil min Integer/MAX_VALUE) quantity (long (/ amount value)))}))
        num-coins (lvar)]
    (run 1 [q]
      (== q {:num-coins num-coins, :coins coins})
      ;; set the domain of num-coins
      (fd/in num-coins (apply fd/interval (possible-total-num-coins-interval coin-set amount)))
      ;; for each coin in the results...
      (everyg
       (fn [{:keys [value n total max-n], :as coin}]
         (all
          ;; give `n` a domain. 0 ≤ n ≤ max-n
          (fd/in n (fd/interval 0 max-n))
          ;; `value` × `n` = `total`
          (fd/* value n total)))
       coins)
      ;; total number of coins must equal `num-coins`
      (sum° (mapv :n coins) num-coins)
      ;; total value of coins must equal `amount`
      (sum° (mapv :total coins) amount))))

(defn format-solution
  "Convert the solution into a nice ordered map with extra info stripped out."
  [{:keys [num-coins coins]}]
  (into (array-map :num-coins num-coins) (for [coin  (sort-by (comp - :value) coins)
                                               :when (pos? (:n coin))]
                                           [(:name coin) (:n coin)])))

(defn make-change
  "Make change for an `amount` (as a floating-point value e.g. a dollar amount) of money using the least possible number
  of coins."
  [amount]
  {:pre [(not (neg? amount))]}
  (let [amount (*round-fractional-amount-fn* (* amount 100))]
    (when-let [[solution] (not-empty (make-change* amount))]
      (format-solution solution))))
