(ns camsaul.logic.examples.subsets
  (:refer-clojure :exclude [==])
  (:require [camsaul.logic.util :as logic.u]
            [clojure.core.logic :refer :all]))

(defn subsets [superset]
  (run* [subset]
    (logic.u/ordered-subset° superset subset)))
