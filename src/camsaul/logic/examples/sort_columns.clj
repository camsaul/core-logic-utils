(ns camsaul.logic.examples.sort-columns
  "From a really old version of `metabase.driver.query-processor.annotate`."
  (:refer-clojure :exclude [==])
  (:require [camsaul.logic.util :as logic.u]
            [clojure.core.logic :refer :all]
            [clojure.core.logic.arithmetic :as ar]
            [clojure.core.logic.fd :as fd]))

(defn- field-name°
  "A relation such that FIELD's name is FIELD-NAME."
  [field field-name]
  (all (trace-lvars "field-name°" field field-name)
       (featurec field {:field-name field-name})))

(defn- make-field-in°
  "Create a relation such that FIELD has an ID matching one of the Field IDs found in FORM."
  [form]
  (let [fields (:fields form)]
    (if-not (seq fields)
      (constantly fail)
      (let [ids-domain (apply fd/domain (sort (distinct (map :field-id fields))))]
        (fn [field]
          (all (trace-lvars "make-field-in°" field ids-domain)
               (fresh [id]
                 (featurec field {:field-id id})
                 (fd/in id ids-domain))))))))

(defn- breakout-field°
  "Create a relation such that a FIELD is present in the `:breakout` clause."
  [{:keys [breakout]}]
  (make-field-in° breakout))

(defn- explicit-fields-field°
  "Create a relation such that a FIELD is present in an explicitly specified `:fields` clause."
  [{:keys [fields-is-implicit fields], :as query}]
  (if fields-is-implicit
    (constantly fail)
    (make-field-in° fields)))

(defn- aggregate-field°
  "Create a relation such that a FIELD is an aggregate field like `:count` or `:sum`."
  [{{ag-type :aggregation-type, ag-field :field} :aggregation}]
  (if-not (contains? #{:avg :count :distinct :stddev :sum} ag-type)
    (constantly fail)
    (let [ag-field (if (contains? #{:count :distinct} ag-type)
                     {:base-type          :IntegerField
                      :field-name         :count
                      :field-display-name "count"
                      :special-type       :number}
                     (-> ag-field
                         (select-keys [:base-type :special-type])
                         (assoc :field-name         (if (= ag-type :distinct) :count
                                                        ag-type)
                                :field-display-name (if (= ag-type :distinct) "count"
                                                        (name ag-type)))))]
      (fn [out]
        (all (trace-lvars "aggregate-field°" out)
             (== out ag-field))))))

(defn- unknown-field°
  "Relation for handling otherwise unknown Fields. If we can't determine why we're seeing a given Field
   (i.e., all other relations like `breakout-field°` and `aggregate-field°` fail), this one will succeed
   as a last resort and bind some fallback properties of the Field, such as giving it a `:base-type` of
   `:UnknownField`. If this relation succeeds, it generally indicates a bug in the query processor."
  [field-name out]
  (all
   (== out {:base-type          :UnknownField
            :special-type       nil
            :field-name         field-name
            :field-display-name field-name})
   (trace-lvars "UNKNOWN FIELD - NOT PRESENT IN EXPANDED QUERY (!)" out)))

(defn- field°
  "Create a relation such that a FIELD is a normal `Field` referenced somewhere in QUERY, or an aggregate
   Field such as a `:count`."
  [query]
  (let [ag-field°         (aggregate-field° query)
        fields            (:fields query)
        field-name->field (zipmap (map :field-name fields) fields)
        normal-field°     (fn [field-name out]
                            (all (trace-lvars "normal-field°" field-name out)
                                 (if-let [field (field-name->field field-name)]
                                   (== out field)
                                   fail)))]
    (fn [field-name field]
      (all (trace-lvars "field°" field-name field)
           (conda
             ((normal-field° field-name field))
             ((ag-field° field)))))))

(def ^:const ^:private field-groups
  "Relative importance of each clause as a source of Fields for the purposes of ordering our results.
   e.g. if a Field comes from a `:breakout` clause, we should return that column first in the results."
  {:breakout        0
   :aggregation     1
   :explicit-fields 2
   :other           3})

(defn- field-group°
  "Create a relation such that OUT is the corresponding value of `field-groups` for FIELD."
  [query]
  (let [breakout° (breakout-field° query)
        agg°      (aggregate-field° query)
        xfields°  (explicit-fields-field° query)]
    (fn [field out]
      (all (trace-lvars "field-group°" field out)
           (conda
             ((breakout° field) (== out (field-groups :breakout)))
             ((agg° field)      (== out (field-groups :aggregation)))
             ((xfields° field)  (== out (field-groups :explicit-fields)))
             (s#                (== out (field-groups :other))))))))

(defn- field-position°
  "A relation such that FIELD's `:position` is OUT. `:position` is the index of the FIELD in its
   source clause, e.g. 2 if it was the third Field in the `:fields` clause where we found it."
  [field out]
  (all (trace-lvars "field-position°" field out)
       (featurec field {:position out})))

(def ^:const ^:private special-type-groups
  "Relative importance of different Field `:special-types` for the purposes of ordering.
   i.e. a Field with special type `:id` should be sorted ahead of all other Fields in the results."
  {:id    0
   :name  1
   :other 2})

(defn- special-type-group°
  "A relation such that OUT is the corresponding value of `special-type-groupds` for FIELD."
  [field out]
  (conda
   ((featurec field {:special-type :id})   (== out (special-type-groups :id)))
   ((featurec field {:special-type :name}) (== out (special-type-groups :name)))
   (s#                                     (== out (special-type-groups :other)))))

(defn- field-name<
  "Create a relation such that the name of Field F1 comes alphabetically before the name of Field F2."
  [query]
  (fn [f1 f2]
    (fresh [name-1 name-2]
      (trace-lvars "field-name<" f1 f2)
      (field-name° f1 name-1)
      (field-name° f2 name-2)
      (logic.u/matches-seq-order° name-1 name-2 (:result-keys query)))))

(defn- clause-position<
  "Create a relation such that Field F1 comes before Field F2 in the clause where they were defined."
  [query]
  (let [group°          (field-group° query)
        breakout-fields (:breakout query)
        fields-fields   (:fields query)]
    (fn [f1 f2]
      (all (trace-lvars "clause-position<" f1 f2)
           (conda
             ((group° f1 (field-groups :breakout))        (logic.u/matches-seq-order° f1 f2 breakout-fields))
             ((group° f1 (field-groups :explicit-fields)) (logic.u/matches-seq-order° f1 f2 fields-fields)))))))

(defmacro <-or-==
  [f & ==-clauses]
  `(all (trace-lvars "fields-sorted°" ~'f1 ~'f2)
        (conda
         ((fresh [v#]
            (~f ~'f1 v#)
            (~f ~'f2 v#)) ~@==-clauses)
         ((fresh [v1# v2#]
            (~f ~'f1 v1#)
            (~f ~'f2 v2#)
            (ar/< v1# v2#)) ~'s#))))

(defn- fields-sorted°
  "Create a relation such that Field F1 should be sorted ahead of Field F2 according to the rules
   listed at the top of this page."
  [query]
  (let [group°      (field-group° query)
        name<       (field-name< query)
        clause-pos< (clause-position< query)]
    (fn [f1 f2]
      (<-or-== group°
               (<-or-== field-position°
                        (conda
                         ((group° f1 (field-groups :other)) (<-or-== special-type-group°
                                                                     (name< f1 f2)))
                         ((clause-pos< f1 f2))))))))

(defn- resolve+order-cols
  "Use `core.logic` to determine the source of the RESULT-KEYS returned by running a QUERY,
   and sort them according to the rules at the top of this page."
  [{:keys [result-keys], :as query}]
  (when (seq result-keys)
    (first (let [fields       (vec (lvars (count result-keys)))
                 known-field° (field° query)]
             (run 1 [q]
               (everyg (fn [[result-key field]]
                         (conda
                           ((known-field°   result-key field))
                           ((unknown-field° result-key field))))
                       (zipmap result-keys fields))
               (logic.u/sorted-permutation° (fields-sorted° query) fields q))))))
