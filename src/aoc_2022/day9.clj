(ns aoc-2022.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn input-from-file []
  (mapv
    (fn [v] [(str (first v)) (Integer/parseInt (second (str/split v #" ")))])
    (-> (slurp (io/resource "rope.txt"))
        (str/split #"\r\n"))))

(def test-input
  [["R" 4]
   ["U" 4]
   ["L" 3]
   ["D" 1]
   ["R" 4]
   ["D" 1]
   ["L" 5]
   ["R" 2]])

(def test-input-2
  [["R" 5]
   ["U" 8]
   ["L" 8]
   ["D" 3]
   ["R" 17]
   ["D" 10]
   ["L" 25]
   ["U" 20]])  ; => 36 (include start)

(def input (input-from-file))

(defn move-head [start-position direction]
  (case direction
    "L" [(dec (first start-position)) (second start-position)]
    "R" [(inc (first start-position)) (second start-position)]
    "D" [(first start-position) (dec (second start-position))]
    "U" [(first start-position) (inc (second start-position))]))

(defn same-or-adjacent? [[x y] [xx yy]]
  (or (= [x y] [xx yy])
      (and (or (= xx (inc x)) (= xx (dec x)) (= x xx))
           (or (= yy (inc y)) (= yy (dec y)) (= y yy)))))

(defn move-directly [tx hx ty hy tail-position]
  (if (= ty hy)
    (if (< tx hx)
      (move-head tail-position "R")
      (move-head tail-position "L"))
    (if (< ty hy)
      (move-head tail-position "U")
      (move-head tail-position "D"))))

(defn move-diagonally [tx hx ty hy tail-position]
  (cond
    (and (< tx hx) (< ty hy))
    (move-head (move-head tail-position "R") "U")

    (and (> tx hx) (< ty hy))
    (move-head (move-head tail-position "L") "U")

    (and (> tx hx) (> ty hy))
    (move-head (move-head tail-position "L") "D")

    (and (< tx hx) (> ty hy))
    (move-head (move-head tail-position "R") "D")))

(defn move-tail [tail-position head-position]
  (let [result
        (if (some? tail-position)
          (if-not (same-or-adjacent? tail-position head-position)
            (let [[tx ty] tail-position
                  [hx hy] head-position]
              (if (or (= tx hx) (= ty hy))
                (move-directly tx hx ty hy tail-position)
                (move-diagonally tx hx ty hy tail-position)))))]
    (if (nil? result)
      tail-position
      result)))

(defn move [[head-position tail-position] [direction steps]]
    (let [moves
          (loop [steps steps
                 head-position head-position
                 tail-position [tail-position]]
            (if (= steps 0)
              [head-position tail-position]
              (recur
                (dec steps)
                (move-head head-position direction)
                (let [tail-moves (move-tail (last tail-position) (move-head head-position direction))]
                  (if (nil? tail-moves)
                    tail-position
                    (conj tail-position tail-moves))))))]
      [(first moves) (into [] (rest (second moves)))]))

(defn move9 [rope [direction steps]]
  (loop [steps steps
         tail-positions []
         rope rope]
    (if (= steps 0)
      [rope tail-positions]
      (recur
        (dec steps)
        (conj tail-positions (last rope))
        (let [head (move-head (first rope) direction)
              rope (into [] (rest rope))]
          (loop [rope rope
                 new-rope [head]]
            (if (empty? rope)
              new-rope
              (recur
                (rest rope)
                (conj new-rope (move-tail (first rope) (last new-rope)))))))))))


(defn part1 []
    (->> (loop [motions input
                moves []]
           (if (empty? motions)
             moves
             (recur
               (rest motions)
               (if (empty? moves)
                 (move [[0 0] [0 0]] (first motions))
                 (let [next-moves (move [(first moves) (last (second moves))] (first motions))]
                   [(first next-moves) (into [] (concat (second moves)
                                                        (second next-moves)))])))))
         second
         distinct
         count))                                           ; => 5929 + 1 (start)

(defn part2 []
  (->> (loop [motions input
              moves [[[0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0]] []]]
         (if (empty? motions)
           moves
           (recur
             (rest motions)
             (let [next-moves (move9 (first moves) (first motions))]
               [(first next-moves) (into [] (concat (second moves)
                                                    (second next-moves)))]))))
       second
       distinct
       count))                                              ; => 2442 + 1 (start)