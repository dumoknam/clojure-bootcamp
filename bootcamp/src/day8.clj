(ns day8
  (:require [clojure.string :as c-str]))
; Handheld Halting

(def input (util/file->lines "day8.txt"))

(defn line->map [line]
  (let [[key val] (c-str/split line #" ")]
    {:op     (keyword key)
     :op-val (Integer/parseInt val)}))

(defn run-operation [i value {:keys [op op-val]}]
  (case op
    :acc [(inc i) (+ op-val value)]
    :jmp [(+ i op-val) value]
    :nop [(inc i) value]
    [i value]))

(defn infinite-loop-value [input]
  (let [operations input]
    (loop [i 0 value 0 visited #{} op (first operations)]
      (if (contains? visited i)
        value
        (let [[next-i next-value] (run-operation i value op)]
          (recur next-i next-value (conj visited i) (nth operations next-i)))))))

; part 1
(->> input
     (map line->map)
     (infinite-loop-value))
;1594

(comment
  (prn input)
  (contains? #{0 1 2 3 4 6 7} 5)
  (run-operation 0 0 {:op "acc" :op-val 47})
  (conj #{} 0)
  (map line->map input)
  (run {:op "acc" :val 2})
  (def input-map (->> input (map line->map)))
  (nth input-map 5))

; part 1 - refactoring
; 상태 + 상태변환 함수 + lazy-seq 형태로 변환

;; ->next-state: S,v => S'
;; reduce -> map/filter/reduce
;; loop/recur -> map/filter/reduce or iterate/drop-while/first

;; iterate function
;(first (drop-while end? (iterate ->next-state initial-state)))
(def initial-state
  {:i       0
   :value   0
   :visited #{}})

(def op-map
  (->> input
       (map line->map)))

(defn ->next-state [{:keys [i value visited]}]
  (let [{:keys [op op-val]} (nth op-map i)]
    (case op
      :acc {:i       (inc i)
            :value   (+ op-val value)
            :visited (conj visited i)}
      :jmp {:i       (+ i op-val)
            :value   value
            :visited (conj visited i)}
      :nop {:i       (inc i)
            :value   value
            :visited (conj visited i)})))

(defn first-run? [{:keys [i _ visited]}]
  (not (contains? visited i)))

; answer
(->> initial-state
     (iterate ->next-state)
     (drop-while first-run?)
     (first)
     (:value))

(comment
  (nth op-map 1)
  (next-state initial-state)
  (take 3 (iterate ->next-state initial-state))
  (take 2 (map first-run? (iterate ->next-state initial-state))))
