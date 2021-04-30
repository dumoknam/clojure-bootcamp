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

(defn value-at-any-op-second-executed [input]
  (let [operations input]
    (loop [i 0 value 0 visited #{} op (first operations)]
      (if (contains? visited i)
        value
        (let [[next-i next-value] (run-operation i value op)]
          (recur next-i next-value (conj visited i) (nth operations next-i)))))))

; part 1
(->> input
     (map line->map)
     (value-at-any-op-second-executed))
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

; -----------------------------------------------------------------------------
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
   :visited []})

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
  (not (some #(= i %) visited)))

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


; part 2
(defn not-acc? [i]
  (not= ((nth op-map i) :op) :acc))

(def candidates
  (->> initial-state
       (iterate ->next-state)
       (drop-while first-run?)
       (first)
       (:visited)
       (filter not-acc?)))
; count = 96

(defn op-swap [op]
  (let [swap-map {:acc :acc, :jmp :nop, :nop :jmp}]
    (swap-map op)))

;; p1 p2의 next-state 함수를 하나로 리팩토링
(defn ->next-state-with-op-swap [{:keys [i value visited swap-i]}]
  (let [{:keys [op op-val]} (nth op-map i)
        operation (if (= i swap-i) (op-swap op) op)]
    (case operation
      :acc {:i       (inc i)
            :value   (+ op-val value)
            :visited (conj visited i)
            :swap-i  swap-i}
      :jmp {:i       (+ i op-val)
            :value   value
            :visited (conj visited i)
            :swap-i  swap-i}
      :nop {:i       (inc i)
            :value   value
            :visited (conj visited i)
            :swap-i  swap-i})))

; 바꿀 인덱스를 포함한 초기 상태 map 생성
(defn generate-state-w-candidate [swap-i]
  (assoc initial-state :swap-i swap-i))

; 계속 실행할 조건 (loop 만나거나 끝나거나)
(defn first-run-or-not-end? [{:keys [i visited]}]
  (and (not (some #(= i %) visited))
       (not (= i (count op-map)))))

(defn run-instructions [start-state]
  (->> start-state
       (iterate ->next-state-with-op-swap)
       (drop-while first-run-or-not-end?)
       (first)))

(defn run-all? [{:keys [i]}]
  (= i (count op-map)))

(->> (map generate-state-w-candidate candidates)
     (map run-instructions)
     (filter run-all?)
     (map :value))
;758

(comment
  (pr candidates)
  (generate-state-w-candidate 0)
  (count candidates)
  (not-acc? 0)
  ((nth op-map 0) :op))
