(ns day9)
;--- Day 9: Encoding Error ---

(def input (util/file->lines "day9.txt"))
(def input-seq
  (->> input
       (map bigint)))

(defn find-2-sum [n s]
  (first (for [x s
               y s
               :when (= n (+ x y))]
           [x y])))

(defn take-n-numbers [take-n s]
  (fn [end-index]
    (let [drop-n (- end-index take-n)]
      (take take-n (drop drop-n s)))))

; 현재 값 앞의 25개 값 가져오기
(def preamble (take-n-numbers 25 input-seq))

(defn invalid? [i]
  (nil? (find-2-sum (nth input-seq i) (preamble i))))

(def input-index (range 25 (count input-seq)))

; part 1. answer
(->> input-index
     (filter invalid?)
     (first))
;(nth input-seq))
; 21806024N

(comment
  ; 25개값 가져오기
  (map #(nth input-seq %) (range 25))
  (contains? (apply sorted-set (map #(nth input-seq %) (range 25))) 20)
  (invalid? 25)
  (find-2-sum 68N (preamble 25)))

; part 2.
; n 을 만드는 연속된 숫자의 first, last 구하기
(defn find-contiguous-set [[max-i n s]]
  (first (for [x-i (range max-i)
               y-i (range (inc x-i) max-i)
               :when (= n (reduce + ((take-n-numbers (- y-i x-i) s) y-i)))]
           [x-i y-i])))

(defn sum-of-min-max [l]
  (+ (apply min l) (apply max l)))

(defn index->input [i]
  [i (nth input-seq i) input-seq])

(def ->input
  (->> input-index
       (filter invalid?)
       (apply index->input)))

; part 2. answer
(->> ->input
     (find-contiguous-set)
     (apply range)
     (map #(nth input-seq %))
     (sum-of-min-max))

(comment
  (apply range (find-contiguous-set [511 21806024N input-seq]))
  ((take-n-numbers (- 10 5) input-seq) 511)
  (get-min-max (map #(nth input-seq %) (range 397 414)))
  (def fff (take-n-numbers (- 10 5) input-seq))
  (= 144 (reduce + (fff 10))))
