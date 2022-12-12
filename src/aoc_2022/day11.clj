(ns aoc-2022.day11)

;; Monkey 0:
;; Starting items: 79, 98
;; Operation: new = old * 19
;; Test: divisible by 23
;; If true: throw to monkey 2
;; If false: throw to monkey 3
;;
;; Monkey 1:
;; Starting items: 54, 65, 75, 74
;; Operation: new = old + 6
;; Test: divisible by 19
;; If true: throw to monkey 2
;; If false: throw to monkey 0
;;
;; Monkey 2:
;; Starting items: 79, 60, 97
;; Operation: new = old * old
;; Test: divisible by 13
;; If true: throw to monkey 1
;; If false: throw to monkey 3
;;
;; Monkey 3:
;; Starting items: 74
;; Operation: new = old + 3
;; Test: divisible by 17
;; If true: throw to monkey 0
;; If false: throw to monkey 1

;; Monkey 0: 20, 23, 27, 26
;; Monkey 1: 2080, 25, 167, 207, 401, 1046
;; Monkey 2:
;; Monkey 3:
(def test-monkeys
  [{:id 0
    :items [79 98]
    :operation (fn [x] (* x 19))
    :test (fn [x] (if (= 0 (mod x 23)) 2 3))
    :inspects 0}
   {:id 1
    :items [54 65 75 74]
    :operation (fn [x] (+ x 6))
    :test (fn [x] (if (= 0 (mod x 19)) 2 0))
    :inspects 0}
   {:id 2
    :items [79 60 97]
    :operation (fn [x] (* x x))
    :test (fn [x] (if (= 0 (mod x 13)) 1 3))
    :inspects 0}
   {:id 3
    :items [74]
    :operation (fn [x] (+ x 3))
    :test (fn [x] (if (= 0 (mod x 17)) 0 1))
    :inspects 0}])

(def monkeys
  [{:id 0
    :items [59 65 86 56 74 57 56]
    :operation (fn [x] (* x 17))
    :test (fn [x] (if (= 0 (mod x 3)) 3 6))
    :inspects 0}
   {:id 1
    :items [63 83 50 63 56]
    :operation (fn [x] (+ x 2))
    :test (fn [x] (if (= 0 (mod x 13)) 3 0))
    :inspects 0}
   {:id 2
    :items [93 79 74 55]
    :operation (fn [x] (+ x 1))
    :test (fn [x] (if (= 0 (mod x 2)) 0 1))
    :inspects 0}
   {:id 3
    :items [86 61 67 88 94 69 56 91]
    :operation (fn [x] (+ x 7))
    :test (fn [x] (if (= 0 (mod x 11)) 6 7))
    :inspects 0}
   {:id 4
    :items [76 50 51]
    :operation (fn [x] (* x x))
    :test (fn [x] (if (= 0 (mod x 19)) 2 5))
    :inspects 0}
   {:id 5
    :items [77 76]
    :operation (fn [x] (+ x 8))
    :test (fn [x] (if (= 0 (mod x 17)) 2 1))
    :inspects 0}
   {:id 6
    :items [74]
    :operation (fn [x] (* x 2))
    :test (fn [x] (if (= 0 (mod x 5)) 4 7))
    :inspects 0}
   {:id 7
    :items [86 85 52 86 91 95]
    :operation (fn [x] (+ x 6))
    :test (fn [x] (if (= 0 (mod x 7)) 4 5))
    :inspects 0}])

(def modulo-for-test-monkeys
  (* 23 19 13 17))

(def modulo-for-monkeys
  (* 3 13 2 11 19 17 5 7))

(defn round [monkeys modulo]
  (loop [monkeys monkeys
         new-monkeys []]
    (if (empty? monkeys)
      new-monkeys
      (recur
        (rest monkeys)
        (let [current-monkey (if (empty? new-monkeys)
                               (first monkeys)
                               (first (filter #(= (:id (first monkeys)) (:id %)) new-monkeys)))]
          (loop [items (:items current-monkey)
                 mod-monkeys (if (empty? new-monkeys) monkeys new-monkeys)]
            (if (empty? items)
              mod-monkeys
              (recur
                (rest items)
                (let [mod-current-monkey (first (filter #(= (:id current-monkey) (:id %)) mod-monkeys))
                      worry-level (mod ((:operation current-monkey) (first items)) modulo)
                      throw-to ((:test current-monkey) worry-level)
                      target-monkey (first (filter #(= throw-to (:id %)) mod-monkeys))
                      target-monkey (update target-monkey :items #(conj % worry-level))
                      mod-current-monkey (-> mod-current-monkey
                                         (update :items #(into [] (rest %)))
                                         (update :inspects inc))]
                  (->> (conj
                         (remove #(#{(:id mod-current-monkey) (:id target-monkey)} (:id %)) mod-monkeys)
                         target-monkey
                         mod-current-monkey)
                       (sort-by :id)
                       (into [])))))))))))

(defn part1 []
  (loop [rounds 20
         monkeys monkeys]
    (if (= rounds 0)
      monkeys
      (recur
        (dec rounds)
        (round monkeys modulo-for-monkeys)))))

(defn part2 []
  (mapv #(select-keys % [:id :inspects])
        (loop [rounds 10000
               monkeys monkeys]
          (if (= rounds 0)
           monkeys
           (recur
             (dec rounds)
             (round monkeys modulo-for-monkeys))))))
