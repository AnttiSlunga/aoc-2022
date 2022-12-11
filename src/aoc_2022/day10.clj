(ns aoc-2022.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn input-from-file []
  (mapv
    (fn [v] [(first (str/split v #" "))
            (if (not (nil? (second (str/split v #" "))))
              (Integer/parseInt (second (str/split v #" "))))])
   (-> (slurp (io/resource "signals.txt"))
       (str/split #"\r\n"))))

(def test-input
  [["addx" 15]
   ["addx" -11]
   ["addx" 6]
   ["addx" -3]
   ["addx" 5]
   ["addx" -1]
   ["addx" -8]
   ["addx" 13]
   ["addx" 4]
   ["noop"]
   ["addx" -1]
   ["addx" 5]
   ["addx" -1]
   ["addx" 5]
   ["addx" -1]
   ["addx" 5]
   ["addx" -1]
   ["addx" 5]
   ["addx" -1]
   ["addx" -35]
   ["addx" 1]])

(defn split-input [input]
  (loop [input input
         modified-input []]
    (if (empty? input)
      modified-input
      (recur
        (rest input)
        (let [command (first input)]
          (if (str/starts-with? (first command) "addx")
            (conj (conj modified-input ["addx"]) command)
            (conj modified-input (first input))))))))

(defn part1 []
  (let [x (atom 1)
        cycle (atom 0)]
    (doall
      (for [command (split-input (input-from-file))]
        (do
          (swap! cycle inc)
          (if (#{20 60 100 140 180 220} @cycle)
            (println "Cycle " @cycle " X " @x " Signal strenght ::" (* @cycle @x)))
          (let [size (count command)]
            (if (and (= 2 size) (not (nil? (second command))))
              (swap! x #(+ (second command) %)))))))
    (println "Done!")))

(defn sprite [x c]
  (if (and (<= c (inc x)) (>= c (dec x))) "#" "."))

(defn position [cycle]
  (cond
    (<= cycle 40)
    (dec cycle)

    (<= cycle 80)
    (dec (- cycle 40))

    (<= cycle 120)
    (dec (- cycle 80))

    (<= cycle 160)
    (dec (- cycle 120))

    (<= cycle 200)
    (dec (- cycle 160))

    :else
    (dec (- cycle 200))))

(defn part2 []
  (let [x (atom 1)
        cycle (atom 0)
        screen (atom "")]
    (doall
      (for [command (split-input (input-from-file))]
        (do
          (swap! cycle inc)
          (swap! screen #(str % (sprite @x (position @cycle))))
          (if (#{40 81 122 163 204 245} (count @screen))
            (swap! screen #(str % "\n")))
          (let [size (count command)]
            (if (and (= 2 size) (not (nil? (second command))))
              (swap! x #(+ (second command) %)))))))
    (map #(println %) (str/split-lines @screen))))