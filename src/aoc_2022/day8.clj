(ns aoc-2022.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn input-from-file []
  (mapv
    (fn [v]
      (mapv #(Integer/parseInt %) (str/split v #"")))
    (-> (slurp (io/resource "trees.txt"))
        (str/split #"\n"))))

(def test-input
  [[3 0 3 7 3]
   [2 5 5 1 2]
   [6 5 3 3 2]
   [3 3 5 4 9]
   [3 5 3 9 0]])

(def input
  (input-from-file)
  #_test-input)

(def last-row
  (dec (count input)))

(def last-column
  (dec (count (first input))))

(defn on-the-edge? [row column]
  (or (or (= row 0) (= row last-row))
      (or (= column 0) (= column last-column))))

(defn visible? [row column tree]
  (let [visible-from-top? (every? true? (for [rowc (range row)]
                                          (< (nth (nth input rowc) column) tree)))
        visible-from-down? (every? true? (for [rowc (range (inc row) (inc last-row))]
                                           (< (nth (nth input rowc) column) tree)))
        visible-from-left? (every? true? (for [columnn (range column)]
                                           (< (nth (nth input row) columnn) tree)))
        visible-from-right? (every? true? (for [columnn (range (inc column) (inc last-column))]
                                            (< (nth (nth input row) columnn) tree)))]
    (or visible-from-top?
        visible-from-down?
        visible-from-left?
        visible-from-right?)))

(defn count-trees [trees]
  (if (empty? trees)
    0
    (loop [trees trees
           count 0]
      (if (or (empty? trees) (>= (first trees) 2))
        (if (empty? trees)
          count
          (inc count))
        (recur (rest trees)
               (inc count))))

    #_(let [_ (println "[" trees "]")
          ind (.indexOf trees 2)]
      (if (= (first trees) 0)
        1
        (if (= ind -1)
          (apply + trees)
          (->> (take (inc ind) trees)
               (apply +)
               dec))))))
(defn scenic-score [row column tree]
  (let [up (for [rowc (reverse (range row))]
             (cond
               (< (nth (nth input rowc) column) tree) 1
               (= (nth (nth input rowc) column) tree) 2
               (> (nth (nth input rowc) column) tree) 3))
        up (count-trees up)
        down (for [rowc (range (inc row) (inc last-row))]
               (cond
                 (< (nth (nth input rowc) column) tree) 1
                 (= (nth (nth input rowc) column) tree) 2
                 (> (nth (nth input rowc) column) tree) 3))
        down (count-trees down)
        left (for [columnn (reverse (range column))]
               (cond
                 (< (nth (nth input row) columnn) tree) 1
                 (= (nth (nth input row) columnn) tree) 2
                 (> (nth (nth input row) columnn) tree) 3))
        left (count-trees left)
        right (for [columnn (range (inc column) (inc last-column))]
                (cond
                  (< (nth (nth input row) columnn) tree) 1
                  (= (nth (nth input row) columnn) tree) 2
                  (> (nth (nth input row) columnn) tree) 3))
        right (count-trees right)]
    (* up down left right)))

(defn part1 []
  (let [trees-on-the-edge (+ (* 2 (count (first input))) (* 2 (- (count input) 2)))
        trees-on-grid (->> input
                           (map-indexed
                             (fn [row v]
                               (map-indexed
                                 (fn [column tree]
                                   (if-not (on-the-edge? row column)
                                     (if (visible? row column tree) 1 0)
                                     0)) v)))
                           flatten
                           (apply +))]
    (+ trees-on-the-edge trees-on-grid)))                   ;=> 1647

(defn part2 []
  (let [scores (->> input
                    (map-indexed
                      (fn [row v]
                        (map-indexed
                          (fn [column tree]
                            (scenic-score row column tree)) v)))
                    flatten
                    (into []))]
    (apply max scores)))                                    ;=> 392080
