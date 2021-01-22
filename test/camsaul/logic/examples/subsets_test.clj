(ns camsaul.logic.examples.subsets-test
  (:require [camsaul.logic.examples.subsets :as subsets]
            [clojure.test :refer :all]))

(deftest subsets-test
  (is [[]]
      (= subsets/subsets []))
  (is [[] [:a]]
      (= subsets/subsets [:a]))
  (is [[] [:a] [:b] [:a :b]]
      (= subsets/subsets [:a :b]))
  (is [[]   [:a]    [:b]    [:a :b]
       [:c] [:a :c] [:b :c] [:a :b :c]]
      (= subsets/subsets [:a :b :c])))
