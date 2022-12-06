(ns aoc-2022.day6
  (:require [clojure.java.io :as io]))

(defn part1 []
  (let [input (slurp (io/resource "packets.txt"))
        found false]
    (loop [found found
           input input
           count 3]
      (if found
        count
        (recur (if (every? #(= 1 %) (vals (frequencies (take 4 input))))
                 true
                 false)
               (rest input)
               (inc count))))))

(defn part2 []
  (let [input (slurp (io/resource "packets.txt"))
        found false]
    (loop [found found
           input input
           count 13]
      (if found
        count
        (recur (if (every? #(= 1 %) (vals (frequencies (take 14 input))))
                 true
                 false)
               (rest input)
               (inc count))))))