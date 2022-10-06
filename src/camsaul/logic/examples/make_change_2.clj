(ns camsaul.logic.examples.make-change-2
  (:require
   [clojure.core.logic :as l]
   [clojure.core.logic.fd :as fd]))

;;; coins is a sequence of coins e.g. `[1 10 10 25]`

(defn amounto
  "A relation such that the value of a sequence of `coins` equals `amount`."
  [coins amount]
  (l/matcha [coins]
    ;; amount of empty list => 0
    ([[]]
     (l/== amount 0))
    ;; amount of list with one element => value of that element
    ([[?coin]]
     (l/== amount ?coin))
    ;; amount of list of (coin . more-coins) => value of coin + recursive value of more-coins
    ([[?coin . ?more-coins]]
     (l/fresh [more-coins-amount]
       (amounto ?more-coins more-coins-amount)
       (fd/+ ?coin more-coins-amount amount)))))

(defn counto
  "A relation such that the number of items in `coll` is `n`."
  [coll n]
  (l/matcha [coll]
    ([[]]
     (l/== n 0))
    ([[_ . ?rest]]
     (l/fresh [rest-count]
       (fd/+ rest-count 1 n)
       (counto ?rest rest-count)))))

(defn sortedo
  "With one arg: a relation such that a collection of numbers `nums` is sorted.

   With two args: a relation such that `sorted` is a sorted version of a collection of numbers `nums`."
  ([nums]
   (l/matcha [nums]
     ([[]]
      l/succeed)
     ([[_]]
      l/succeed)
     ([[?x . ?more]]
      (l/fresh [y]
        (l/firsto ?more y)
        (fd/<= ?x y))
      (sortedo ?more))))

  ([sorted nums]
   (if-not (l/lvar? nums)
     (l/== sorted (sort nums))
     (l/all
      (l/permuteo nums sorted)
      (sortedo sorted)))))



(defn everyg*
  "This is the same as [[clojure.core.logic/everyg]] but it works on lists that lvars as opposed to only working on
  grounded lists."
  [goal-fn coll]
  (if (not (l/lvar? coll))
    (l/everyg goal-fn coll)
    (l/matcha [coll]
      ([[]]
       l/succeed)
      ([[?x . ?more]]
       (goal-fn ?x)
       (everyg* goal-fn ?more)))))

(def ^:private max-coin-size
  "If `coin-set` is an lvar, the maximum coin size to try to use. This can be higher but it might make things SLOOOW."
  1000)

(defn coin-set-o
  "A relation such that `coin-set` is a valid set of coins."
  [coin-set]
  (l/all
   (l/distincto coin-set)
   (sortedo coin-set)
   (everyg* (fn [coin]
              (fd/in coin (fd/interval 1 max-coin-size)))
            coin-set)))

(defn coin-set-domain-o
  "A relation such that all `coins` are in the domain of the `coin-set`."
  [coins coin-set]
  (l/all
   (if (l/lvar? coin-set)
     (coin-set-o coin-set)
     l/succeed)
   (everyg* (fn [coin]
              (if (l/lvar? coin-set)
                (l/member1o coin coin-set)
                (fd/in coin (apply fd/domain coin-set))))
            coins)))

(defn change-using-num-coins-o
  "A relation such that all `coins` is a sorted sequence of coins in `coin-set` of length `num-coins` that add up to
  `amount`."
  [coins amount coin-set num-coins]
  (l/all
   (if (l/lvar? num-coins)
     (counto coins num-coins)
     (l/== coins (l/lvars num-coins)))
   (coin-set-domain-o coins coin-set)
   (amounto coins amount)
   (sortedo coins)))

(defn changeo
  "A relation such that `coins` is a sorted sequence of coins in `coin-set` adding up to `amount`. Solutions with less
  total numbers of coins are returned first."
  ([coins amount coin-set]
   (changeo coins amount coin-set 0))

  ([coins amount coin-set min-coins]
   (l/conde
    ;; try making change with num-coins = min-coins
    ((change-using-num-coins-o coins amount coin-set min-coins))
    ;; otherwise if min-coins + 1 is <= amount recursively try making change with num-coins + 1
    ((fd/< min-coins amount)
     (let [min-coins' (inc min-coins)]
       (changeo coins amount coin-set min-coins'))))))

(defn make-change [amount coin-set]
  (or (first (l/run 1 [coins]
               (changeo coins amount coin-set)))
      (throw (ex-info "No possible ways to make change"
                      {:amount amount, :coin-set coin-set}))))
