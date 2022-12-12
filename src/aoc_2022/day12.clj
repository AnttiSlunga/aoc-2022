(ns aoc-2022.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn file-input []
  (slurp (io/resource "hills.txt")))

(def test-input
  "Sabqponm
   abcryxxl
   accszExk
   acctuvwj
   abdefghi")

(def input
  (mapv
    (fn [v]
      (str/split (str/trim v) #""))
    (-> (file-input)
        (str/split #"\n"))))

(def last-row
  (dec (count input)))

(def last-column
  (dec (count (first input))))

(defn find-start []
  (->> input
       (map-indexed
         (fn [row v]
           (map-indexed
             (fn [column hill]
               (if (= "S" hill)
                 [row column])) v)))
       flatten
       (remove nil?)
       (into [])))

(defn next-height [current-height]
  (-> (str/split (str (apply str (map char (range 97 123))) "E") (re-pattern current-height))
      rest first (get 0) str))

(defn get-height [[row column]]
  (if (= (find-start) [row column])
    ["a" [row column]]
    (if-not (or (< row 0) (> row last-row) (< column 0) (> column last-column))
      [(nth (nth input row) column) [row column]])))

(def directions
  [(fn [row column] (get-height [(dec row) column]))
   (fn [row column] (get-height [(inc row) column]))
   (fn [row column] (get-height [row (dec column)]))
   (fn [row column] (get-height [row (inc column)]))])

(defn get-next-position [[row column] previous-position]
  (let [current-height (first (get-height [row column]))
        next-height (next-height current-height)
        higher (remove nil? (mapv #(if (= (first (% row column)) next-height) (second (% row column))) directions))
        same (remove nil? (mapv #(if (= (first (% row column)) current-height) (second (% row column))) directions))
        same (if (> (count same) 1) (remove #(= % previous-position) same) same)]
    (if (not-empty higher)
      (first higher)
      (first same)
      #_(if (= (count higher) 1)
        (first higher))
      #_(if (= (count same) 1)
        (first same)))))

(defn part1 []
  (let [start-position (find-start)
        moves (loop [height "a"
                     moves [start-position]]
                (if (or (= height "E") (= 10 (count moves)))
                  moves
                  (recur
                    (first (get-height (get-next-position (last moves) (last (pop moves)))))
                    (conj moves (get-next-position (last moves) (last (pop moves)))))))
        _ (println (last moves))
        ]
    #_(->> moves
         count
         dec)
    moves))

