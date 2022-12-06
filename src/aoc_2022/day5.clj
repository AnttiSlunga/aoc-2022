(ns aoc-2022.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; [T]     [D]         [L]
;; [R]     [S] [G]     [P]         [H]
;; [G]     [H] [W]     [R] [L]     [P]
;; [W]     [G] [F] [H] [S] [M]     [L]
;; [Q]     [V] [B] [J] [H] [N] [R] [N]
;; [M] [R] [R] [P] [M] [T] [H] [Q] [C]
;; [F] [F] [Z] [H] [S] [Z] [T] [D] [S]
;; [P] [H] [P] [Q] [P] [M] [P] [F] [D]
;;  1   2   3   4   5   6   7   8   9
(def crates
  {1 ["P" "F" "M" "Q" "W" "G" "R" "T"]
   2 ["H" "F" "R"]
   3 ["P" "Z" "R" "V" "G" "H" "S" "D"]
   4 ["Q" "H" "P" "B" "F" "W" "G"]
   5 ["P" "S" "M" "J" "H"]
   6 ["M" "Z" "T" "H" "S" "R" "P" "L"]
   7 ["P" "T" "H" "N" "M" "L"]
   8 ["F" "D" "Q" "R"]
   9 ["D" "S" "C" "N" "L" "P" "H"]})
;; move 3 from 8 to 9
;; move 2 from 2 to 8
;; move 5 from 4 to 2
;; move 7 from 1 to 4
;; ....
(defn move-crates [[times from to] crates]
  (if (= 0 times)
    crates
    (recur [(dec times) from to]
           (-> crates
               (assoc to (conj (get crates to)
                               (peek (get crates from))))
               (assoc from (pop (get crates from)))))))

(defn move-multiple-crates [[count from to] crates]
  (-> crates
      (assoc to (into []
                      (concat (get crates to)
                              (take-last count (get crates from)))))
      (assoc from (into [] (drop-last count (get crates from))))))

(defn part1 []
  (let [moves (->> (-> (slurp (io/resource "crates.txt"))
                       (str/split #"\n"))
                   (mapv (fn [l] (let [row (str/split l #" ")]
                                   [(Integer/parseInt (nth row 1))
                                    (Integer/parseInt (nth row 3))
                                    (Integer/parseInt (nth row 5))]))))]
    (sort (loop [crates crates
                 moves moves]
            (if (empty? moves)
              crates
              (recur (move-crates (first moves) crates)
                     (rest moves)))))))

(defn part2 []
  (let [moves (->> (-> (slurp (io/resource "crates.txt"))
                       (str/split #"\n"))
                   (mapv (fn [l] (let [row (str/split l #" ")]
                                   [(Integer/parseInt (nth row 1))
                                    (Integer/parseInt (nth row 3))
                                    (Integer/parseInt (nth row 5))]))))]
    (sort (loop [crates crates
                 moves moves]
            (if (empty? moves)
              crates
              (recur (move-multiple-crates (first moves) crates)
                     (rest moves)))))))