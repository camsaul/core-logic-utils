(ns camsaul.logic.fd-util
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]))

(defn count°
  "A relation such that `cnt` is the count of items in `l`."
  [l cnt]
  (matcha [l]
    ([[]]
     (== cnt 0))
    ([[_ . ?rest]]
     (fresh [rest-count]
       (count° ?rest rest-count)
       (fd/+ rest-count 1 cnt)))))

(defn nth°
  "A relation such that `item` is the item at index `i` of `l`."
  [l i item]
  (matcha [l]
    ([[]]
     u#)
    ([[item . _]]
     (== i 0))
    ([[_ . ?more]]
     (fresh [i-more]
       (fd/- i 1 i-more)
       (nth° ?more i-more item)))))

(defn div°
  "Integer division relation: `num/div = x`."
  [num div x]
  (case (mapv lvar? [num div x])
    [false false false]
    (== (long (/ num div)) x)

    [true false false]
    (fd/in num (fd/interval (* div x) (+ (* div x) (dec div))))

    [false true false]
    (== div (long (/ num x)))

    [false false true]
    (== x (long (/ num div)))

    (conda
     ((fd/> div num)
      (== x 0))

     ((fd/== div num)
      (== x 1))

     (s#
      (trace-lvars "div°" num div x)
      u#))))

(defn rem°
  "A relation such that `remainder` is the remainder of `num / div`."
  [num div remainder]
  (letfn [(rem°* [num div remainder]
            (fresh [remainder+1 div*remainder div*remainder+1]
              (fd/+ remainder 1 remainder+1)
              (fd/* div remainder div*remainder)
              (fd/* div remainder+1 div*remainder+1)
              (fd/>= num div*remainder)
              (fd/<= num div*remainder+1)))]
    (case (mapv lvar? [num div remainder])
      [false false false]
      (== (rem num div) remainder)
      ;; optimized cases if we only have one lvar
      [true false false]
      (fd/in num (fd/interval (* div remainder) (* div (inc remainder))))

      [false true false]
      (== div (long (/ num remainder)))

      [false false true]
      (== (rem num div) remainder)

      ;; if num is concrete we can give div and remainder constraints
      [false true true]
      (all
       (fd/in div (fd/interval 0 (inc num)))
       (fd/in remainder (fd/interval 0 (inc num)))
       (rem°* num div remainder))

      ;; any other of lvars can only be done IFF some combination (which ones?) of num/div/remainder have domains
      (rem°* num div remainder))))
