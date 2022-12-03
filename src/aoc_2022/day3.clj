(ns aoc-2022.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn points []
  (merge
    (->> (map char (range 97 123))
         (map-indexed (fn [i c] {c (+ 1 i)}))
         (apply conj))
    (->> (map char (range 65 91))
         (map-indexed (fn [i c] {c (+ 27 i)}))
         (apply conj))))

(defn part1 []
  (let [sacks (-> (slurp (io/resource "sacks"))
                  (str/split #"\n"))]
    (->> sacks
         (mapv
           (fn [sack] (let [[left right] (mapv #(apply str %) (split-at (/ (count sack) 2) sack))]
                        (->> right
                             (map #(if (str/includes? left (str %)) (get (points) %)))
                             (remove nil?)
                             distinct))))
         flatten
         (apply +))))

(defn part2 []
  (let [sacks (partition 3 (-> (slurp (io/resource "sacks"))
                               (str/split #"\n")))]
    (->> sacks
         (mapv
           (fn [[f s t]]
             (->> f
                  (map #(if (str/includes? s (str %))
                          (if (str/includes? t (str %))
                            (get (points) %))))
                  (remove nil?)
                  ;distinct
                  )))
         flatten
         (apply +))))