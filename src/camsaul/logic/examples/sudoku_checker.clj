(ns camsaul.logic.examples.sudoku-checker
  (:refer-clojure :exclude [==])
  (:require [camsaul.logic.fd-util :as fd.u]
            [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]))

(defn group-solved°
  "A relation such that a group (e.g. a row, col, or square) (represented as a vector of nine numbers) is valid."
  [group]
  (all
   ;; every number must be in 1..9
   (everyg
    (fn [n]
      (fd/in n (fd/interval 1 9)))
    group)
   ;; group must be distinct
   (fd/distinct group)
   ;; group must have 9 members
   (fd.u/count° group 9)))

(defn cell° [board row-num col-num cell]
  (fresh [row]
    (fd.u/nth° board row-num row)
    (fd.u/nth° row col-num cell)))

(defn row°
  [board row-num row]
  (fd.u/nth° board row-num row))

(defn col°
  [board col-num col]
  (let [cells (vec (lvars 9))]
    (all
     (== col cells)
     (everyg
      (fn [[row-num cell]]
        (cell° board row-num col-num cell))
      (zipmap (range 0 9) cells)))))

(defn square° [board square-num square]
  (let [cells    (vec (lvars 9))
        col-nums (vec (lvars 3))
        row-nums (vec (lvars 3))]
    (fresh [square-num-div-by-3
            start-row
            square-num-div-by-3-rem
            start-col]
      (fd.u/div° square-num 3 square-num-div-by-3)
      (fd.u/rem° square-num 3 square-num-div-by-3-rem)
      (fd/* square-num-div-by-3 3 start-row)
      (fd/+ start-row 0 (nth row-nums 0))
      (fd/+ start-row 1 (nth row-nums 1))
      (fd/+ start-row 2 (nth row-nums 2))
      (fd/* square-num-div-by-3-rem 3 start-col)
      (fd/+ start-col 0 (nth col-nums 0))
      (fd/+ start-col 1 (nth col-nums 1))
      (fd/+ start-col 2 (nth col-nums 2))
      (== square cells)
      (everyg
       (fn [[[row-num col-num] cell]]
         (cell° board row-num col-num cell))
       (zipmap (for [row-num row-nums
                     col-num col-nums]
                 [row-num col-num])
               cells)))))

(defn board-solved°
  "A relation such that `board` is a solved correctly"
  [board]
  (all
   (everyg
    (fn [row-num]
      (fresh [row]
        (row° board row-num row)
        (group-solved° row)))
    (range 0 9))
   (everyg
    (fn [col-num]
      (fresh [col]
        (col° board col-num col)
        (group-solved° col)))
    (range 0 9))
   (everyg
    (fn [square-num]
      (fresh [square]
        (square° board square-num square)
        (group-solved° square)))
    (range 0 9))))

(defn solved? [board]
  (first (run 1 [q]
           (== q board)
           (board-solved° board))))

;; sample correctly-solved board
(def solved-board
  [[5 3 4  6 7 8  9 1 2]
   [6 7 2  1 9 5  3 4 8]
   [1 9 8  3 4 2  5 6 7]

   [8 5 9  7 6 1  4 2 3]
   [4 2 6  8 5 3  7 9 1]
   [7 1 3  9 2 4  8 5 6]

   [9 6 1  5 3 7  2 8 4]
   [2 8 7  4 1 9  6 3 5]
   [3 4 5  2 8 6  1 7 9]])

;; create a copy of the board with the number in 2,2 changed to 3
(def bad-board
  (assoc-in solved-board [2 2] 3))
