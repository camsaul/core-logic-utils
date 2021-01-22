(ns camsaul.logic.examples.dedupe-joins
  "From `metabase.models.params.chain-filter.dedupe-joins`"
  (:require [camsaul.logic.util :as logic.u]
            [clojure.core.logic :as l]
            [clojure.set :as set]))

(defn- lhso
  "A relation such that the left-hand side (LHS) of `join` is `lhs`."
  [join lhs]
  (l/featurec join {:lhs {:table lhs}}))

(defn- rhso
  "A relation such that the right-hand side (RHS) of `join` is `rhs`."
  [join rhs]
  (l/featurec join {:rhs {:table rhs}}))

(defn- has-joino
  "A relation such that `joins` has a join whose RHS is `rhs`."
  [joins rhs]
  (logic.u/anyg #(rhso % rhs) joins))

(defn- parent-joino
  "True if `join-1` can be considered a 'parent' of `join-2` -- if the Table made available by `join-1` (its RHS) is
  needed for `join-2` (its LHS)."
  [join-1 join-2]
  (l/fresh [id]
    (rhso join-1 id)
    (lhso join-2 id)))

(defn- parent-beforeo
  "A relationship such that the parent join of `join` appears before it in `joins`."
  [joins join]
  (l/fresh [joins-before parent]
    (logic.u/list-before° joins joins-before join)
    (parent-joino parent join)
    (l/membero parent joins-before)))

(defn- distinct-rhso
  "A relationship such that all RHS tables in `joins` are distinct."
  [joins]
  (let [rhses (vec (l/lvars (count joins)))]
    (dorun (map (fn [join rhs]
                  (rhso join rhs))
                joins rhses))
    (l/all
     (l/distincto rhses))))

(defn dedupe-joins
  "Remove unnecessary joins from a collection of `in-joins`.

  `keep-ids` = the IDs of Tables that we want to keep joins for. Joins that are not needed to keep these Tables may be
  removed."
  [source-id in-joins keep-ids]
  ;; we can't keep any joins that don't exist in `in-joins`, so go ahead and remove IDs for those joins if they're not
  ;; present
  (let [keep-ids (set/intersection (set keep-ids)
                                   (set (map #(get-in % [:rhs :table]) in-joins)))]
    (first
     (some
      seq
      (for [num-joins (range (count keep-ids) (inc (count in-joins)))]
        (let [out-joins (vec (l/lvars num-joins))]
          (l/run 1 [q]
            (l/== q out-joins)
            ;; every join in out-joins must be present in the original non-deduped set of joins
            (l/everyg (fn [join]
                        (l/membero join in-joins))
                      out-joins)
            ;; no duplicate joins (this is mostly for optimization since we also deduplicate RHSes below)
            (l/distincto out-joins)
            ;; a join for every rhs must be present
            (l/everyg (fn [id]
                        (has-joino out-joins id))
                      keep-ids)
            ;; no duplicate rhses
            (distinct-rhso out-joins)
            ;; joins must be in order (e.g. parent join must come first)
            (l/everyg (fn [join]
                        (l/conda
                         ;; either the LHS is the source Table...
                         ((lhso join source-id))
                         ;; or its LHS must have already been joined
                         ((parent-beforeo out-joins join))))
                      out-joins))))))))
