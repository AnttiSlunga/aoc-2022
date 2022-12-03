(ns aoc-2022.day1
  (:require [clojure.java.io :as io]))

(defn parse [s]
  (if (string? s) (Integer/parseInt s) s))

(defn most-calories []
  (let [calories (-> (slurp (io/resource "calories"))
                     (clojure.string/split #"\n\n"))]
    (->> calories
         (mapv #(->> (clojure.string/split % #"\n")
                              (mapv parse)
                              (reduce +)))
         (apply max))))

(defn top3-calories []
  (let [calories (-> (slurp (io/resource "calories"))
                     (clojure.string/split #"\n\n"))]
    (->> calories
         (mapv #(->> (clojure.string/split % #"\n")
                     (mapv parse)
                     (reduce +)))
         (sort >)
         (take 3)
         (apply +))))
