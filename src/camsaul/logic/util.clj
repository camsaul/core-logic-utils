(ns camsaul.logic.util
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

(defn anyg
  "A psuedo-relation such that goal `g` succeeds for at least one item in `coll`."
  [g coll]
  (conda
   ((fresh [head]
      (firsto coll head)
      (g head)))
   ((fresh [more]
      (resto coll more)
      (anyg g more)))))

(defn list-before°
  "A relation such that `sublist` is all items in `lst` up to (but not including) `item`."
  [lst sublist item]
  (matcha [lst sublist]
    ([[] []])
    ([[item . _] []])
    ([[?x . ?list-more] [?x . ?sublist-more]]
     (list-before° ?list-more ?sublist-more item))))

(defna butlast°
  "A relation such that `bustlastv` is all items but the last item `lastv` of list `l`."
  [butlastv lastv l]
  ([[]   ?x [?x]])
  ([_    _  [?x . ?more]]
   (fresh [more-butlast]
     (butlast° more-butlast lastv ?more)
     (conso ?x more-butlast butlastv))))

(defna split°
  "A relation such that `half1` and `half2` are even divisions of list `l`. If `l` has an odd number of items, `half1`
  will have one more item than `half2`."
  [half1 half2 l]
  ([[]   []   []])
  ([[?x] []   [?x]])
  ([[?x] [?y] [?x ?y]])
  ([[?x ?y . ?more-half1-butlast] [?more-half1-last . ?more-half2] [?x ?y . ?more]]
   (fresh [more-half1]
     (split° more-half1 ?more-half2 ?more)
     (butlast° ?more-half1-butlast ?more-half1-last more-half1))))

(defn sorted-into°
  "A relation such that `out` is the list `l` with `v` sorted into it doing comparisons with `pred-f`."
  [pred-f l v out]
  (matche [l]
    ([[]]
     (== out [v]))

    ([[?x . ?more]]
     (conda
      ((pred-f v ?x) (conso v (lcons ?x ?more) out))
      (s#            (fresh [more]
                       (sorted-into° pred-f ?more v more)
                       (conso ?x more out)))))))

(defna sorted-permutation°
  "A relation such that `out` is a permutation of `l` where all items are sorted by `pred-f`."
  [pred-f l out]
  ([_ [] []])

  ([_ [?x . ?more] _]
   (fresh [more]
     (sorted-permutation° pred-f ?more more)
     (sorted-into° pred-f more ?x out))))

(defn matches-seq-order°
  "A relation such that `v1` is present and comes before `v2` in collection `l`."
  [v1 v2 l]
  (conda
   ;; This is just an optimization for cases where L isn't a logic var; it's much faster <3
   ((nonlvaro l)
    ((fn -ordered° [[item & more]]
       (conda
        ((== v1 item)         s#)
        ((== v2 item)         fail)
        ((when (seq more) s#) (-ordered° more))))
     l))

   (s#
    (conda
     ((firsto l v1))
     ((firsto l v2) fail)
     ((fresh [more]
        (resto l more)
        (matches-seq-order° v1 v2 more)))))))

(defn ordered-subset°
  "A relation such that `subset` is a subset of `superset` with elements in the same order."
  [superset subset]
  ;; subset is a subset of superset (with the same order) if...
  (matche [superset subset]
    ;; subset is empty
    ([_ []])
    ;; subset and superset both start with the same element, and (rest subset) is a subset of (rest superset)
    ([[?head . ?superset-tail] [?head . ?subset-tail]]
     (ordered-subset° ?superset-tail ?subset-tail))
    ;; subset is a subset of (rest superset)
    ([[_ . ?tail] [_ . _]]
     (ordered-subset° ?tail subset))))
