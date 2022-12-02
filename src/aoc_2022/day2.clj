(ns aoc-2022.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; A for Rock, B for Paper, and C for Scissors
;; X for Rock, Y for Paper, and Z for Scissors.
;; 1 for Rock, 2 for Paper, and 3 for Scissors
;; 0 if you lost, 3 if the round was a draw, and 6 if you won

(def results
  {"X" {"A" :tie "B" :loss "C" :won}
   "Y" {"A" :won "B" :tie "C" :loss}
   "Z" {"A" :loss "B" :won "C" :tie}})

(def scores
  {:won 6 :tie 3 :loss 0})

(def selection-score
  {"X" 1 "Y" 2 "Z" 3})

(defn points []
  (let [strategy (-> (slurp (io/resource "guide"))
                     (str/split #"\n"))]
    (->> strategy
         (mapv
           (fn [row] (let [keys (str/split row #" ")
                           e (first keys)
                           m (second keys)
                           result (get-in results [m e])
                           score (+ (scores result) (selection-score m))]
                       (println "enemy " e " me " m " result " result " score " score)
                       score)))
         (apply +))))

;; X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win.

(def choose-for-result
  {"X" {"A" "Z" "B" "X" "C" "Y"}
   "Y" {"A" "X" "B" "Y" "C" "Z"}
   "Z" {"A" "Y" "B" "Z" "C" "X"}})

(defn points2 []
  (let [strategy (-> (slurp (io/resource "guide"))
                     (str/split #"\n"))]
    (->> strategy
         (mapv
           (fn [row] (let [keys (str/split row #" ")
                           e (first keys)
                           expected-result (second keys)
                           m (get-in choose-for-result [expected-result e])
                           result (get-in results [m e])
                           score (+ (scores result) (selection-score m))]
                       (println "enemy " e " me " m " result " result " score " score)
                       score)))
         (apply +))))