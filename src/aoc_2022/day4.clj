(ns aoc-2022.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn overlaps? [[a1 a2] [b1 b2]]
  (if (or (and (>= b1 a1) (<= b2 a2))
          (and (>= a1 b1) (<= a2 b2)))
    true))

(defn overlaps-partly? [[a1 a2] [b1 b2]]
  (if (or (and (>= b1 a1) (<= b1 a2))
          (and (>= a1 b1) (<= a1 b2)))
    true))

(defn part1 []
  (let [sections (-> (slurp (io/resource "sections.txt"))
                     (str/split #"\r\n"))]
    (->> sections
         (mapv
           (fn [section]
             (let [arange (clojure.string/split section #",")
                   frange (mapv #(Integer/parseInt %) (clojure.string/split (first arange) #"-"))
                   srange (mapv #(Integer/parseInt %) (clojure.string/split (second arange) #"-"))]
               (if (overlaps? frange srange) 1 0))))
         (apply +))))

(defn part2 []
  (let [sections (-> (slurp (io/resource "sections.txt"))
                     (str/split #"\r\n"))]
    (->> sections
         (mapv
           (fn [section]
             (let [arange (clojure.string/split section #",")
                   frange (mapv #(Integer/parseInt %) (clojure.string/split (first arange) #"-"))
                   srange (mapv #(Integer/parseInt %) (clojure.string/split (second arange) #"-"))]
               (if (or (overlaps? frange srange) (overlaps-partly? frange srange)) 1 0))))
         (apply +))))