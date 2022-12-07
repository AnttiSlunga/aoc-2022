(ns aoc-2022.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-input []
  (-> (slurp (io/resource "terminal.txt"))
      (str/split #"\n")))

(defn test-input []
  ["$ cd /"
   "$ ls"
   "dir a"
   "14848514 b.txt"
   "8504156 c.dat"
   "dir d"
   "$ cd a"
   "$ ls"
   "dir e"
   "29116 f"
   "2557 g"
   "62596 h.lst"
   "$ cd e"
   "$ ls"
   "584 i"
   "$ cd .."
   "$ cd .."
   "$ cd d"
   "$ ls"
   "4060174 j"
   "8033020 d.log"
   "5626152 d.ext"
   "7214296 k"])

(defn dir? [value]
  (str/starts-with? value "dir"))

(defn cd-command? [value]
  (str/starts-with? value "$ cd"))

(defn command? [value]
  (str/starts-with? value "$"))

(defn file? [value]
  (not (or (dir? value) (command? value))))

(defn make-path [location]
  (keyword (reduce (fn [v a] (if (= "/" v)
                               (str v a)
                               (str v "/" a))) location)))

(defn folders [input]
  (loop [input input
         location []
         output {}]
    (if (empty? input)
      output
      (recur (rest input)
             (let [command (first input)]
               (if (cd-command? command)
                 (let [dir (last (str/split command #" "))]
                   (if (= ".." dir)
                     (pop location)
                     (conj location dir)))
                 location))
             (let [command (first input)]
               (cond
                 (file? command)
                 (loop [output output
                        location location]
                   (if (empty? location)
                     output
                     (recur
                       (assoc output (make-path location)
                                     (+ (get output (make-path location) 0) (Integer/parseInt (first (str/split command #" ")))))
                       (pop location))))

                 :else
                 output))))))

(defn part1 []
  (let [input (get-input)]
    (->> (folders input)
         vals
         (mapv #(if (<= % 100000) % 0))                     ;;1419174 1506020 1778099
         (apply +))))

(defn part2 []
  (let [disk-size 70000000
        used-space (:/ (folders (get-input)))
        required-space (- 30000000 (- disk-size used-space))]
    (->> (folders (get-input))
         (filter #(>= (val %) required-space))
         (mapv val)
         sort
         first)))                                           ;; => 1623571