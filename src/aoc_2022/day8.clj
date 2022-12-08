(ns aoc-2022.day8
  (:require [clojure.java.io :as io]))

(defn input []
  (slurp (io/resource "trees.txt")))

(def test-input
  [[3 0 3 7 3]
   [2 5 5 1 2]
   [6 5 3 3 2]
   [3 3 5 4 9]
   [3 5 3 9 0]])

(def last-row
  (dec (count test-input)))

(def last-column
  (dec (count (first test-input))))

(defn on-the-edge? [row column]
  (or (or (= row 0) (= row last-row))
      (or (= column 0) (= column last-column))))

(defn visible? [row column tree]
  (let [visible-from-top? (every? true? (for [rowc (range row)]
                                          (< (nth (nth test-input rowc) column) tree)))
        visible-from-down? (every? true? (for [rowc (range (inc row) (inc last-row))]
                                           (< (nth (nth test-input rowc) column) tree)))
        visible-from-left? (every? true? (for [columnn (range column)]
                                           (< (nth (nth test-input row) columnn) tree)))
        visible-from-right? (every? true? (for [columnn (range (inc column) (inc last-column))]
                                            (< (nth (nth test-input row) columnn) tree)))]
    (or visible-from-top?
        visible-from-down?
        visible-from-left?
        visible-from-right?)))

(defn part1 []
  (->> test-input
       (map-indexed
         (fn [row v]
           (map-indexed
             (fn [column tree]
               (if-not (on-the-edge? row column)
                 (if (visible? row column tree) 1 0)
                 "^")) v)))))
