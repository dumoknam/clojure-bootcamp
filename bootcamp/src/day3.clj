(ns day3
  (:require [clojure.java.io :as io]))

;; part 1 리팩토링 전
(def input (-> "day3.txt"
               (io/resource)
               (slurp)
               (clojure.string/split-lines)))

;; 전달받은 글자가 tree: \# 인지 체크
#(= \# %1)
;; example
(prn (str "char = tree? " (#(= \# %1) \.)))

;; n번째 글자가 tree 인지 체크 : char seq & index 전달
#(= \# (nth %1 %2))
;; example
(prn (#(= \# (nth %1 %2)) "..#..#" 1))
(prn (#(= \# (nth %1 %2)) "..#..#" 2))

;; 한 줄의 글자수 계산
;; (def ch-per-line (count (first input)))
(def ch-per-line 31)

;; 특정 line 의 index 계산
(defn get-index [line-number]
  (mod (* line-number 3) ch-per-line))

;; input 라인 수 만큼 index 데이터 계산해놓기
;; -> 요게 기존 코딩 방식과 달라진 생각인 것 같음.
;; -> 변수를 두고 업데이트 하려고 했으나 실패.
(map #(get-index %) (range (count input)))

;; 각 라인의 각 인덱스별로 글자가 tree 인지 판단
(map #(= \# (nth %1 %2))
     input
     (map #(get-index %) (range (count input))))

;; true는 1로 false는 0으로 바꿔서
(map #(if (true? %) 1 0)
     (map #(= \# (nth %1 %2))
          input
          (map #(get-index %) (range (count input)))))

;; 더하자!
(#(reduce + %)
  (map #(if (true? %) 1 0)
       (map #(= \# (nth %1 %2))
            input
            (map #(get-index %) (range (count input))))))
;; 270!
;; ---------- ---------- ---------- ---------- ----------

;; part 1 리팩토링 후
(def day3-input (-> "day3.txt"
                    (io/resource)
                    (slurp)
                    (clojure.string/split-lines)))
;; 한 줄의 글자수 계산 -> 31
(def char-per-line (count (first day3-input)))

;; 전체 라인 수 계산
(def number-of-lines (count day3-input))

;; 각 line 에서 확인해야 할 문자의 index 계산
(defn index-of-line [line-number]
  (mod (* line-number 3) char-per-line))

;; day3-input 라인 수 만큼 index 데이터 계산해놓기
(def indexes
  (map index-of-line (range number-of-lines)))

;; 각 라인의 각 인덱스별로 글자가 tree 인 것만 filter 해서 개수 세기
(->> (map nth day3-input indexes)
     (filter #(= \# %))
     (count))
;; 270!

;;;; part 2 가 있을줄이야 ;;;;
; Right 1, down 1.
; Right 3, down 1.
; Right 5, down 1.
; Right 7, down 1.
; Right 1, down 2.
; 5개 함수를 만들어서 하나씩 적용시키고 결과값 곱하기

; case 1. right 1/3/5/7, down 1
(defn get-index-down1 [[line-number right]]
  (mod (* line-number right) char-per-line))

; case 2. right 1, down 2
(defn get-index-down2 [[line-number]]
  (if (even? line-number)
    (mod (/ line-number 2) char-per-line)
    -1))

; case 1 + case 2
(defn index-to-read [[line-number right down1?]]
  (cond
    (= down1? :down1) (get-index-down1 [line-number right])
    :else (get-index-down2 [line-number])))

(defn make-index-inputs [[right down?]]
  (map vector (range number-of-lines)
       (repeat number-of-lines right)
       (repeat number-of-lines down?)))

; line 마다 오른쪽, 아래 몇칸씩 움직이는지 기록한 list 를 5가지 경우 모두 만들기
(def *5-index-inputs
  (map make-index-inputs [[1 :down1] [3 :down1] [5 :down1] [7 :down1] [1 :down2]]))

; 한가지 경우에 대해 확인해야 할 글자들의 리스트 만들기
(defn char-list [*index]
  (->> *index
       (map index-to-read)
       (map #(nth %1 %2 false) day3-input)))

; 한가지 경우에 대한 tree 개수 계산함수
(defn solution [*index]
  (->> *index
       (char-list)
       (filter #(= \# %))
       count))

; 5가지 경우의 값들을 곱해서 최종 결과 계산
(->> *5-index-inputs
     (map solution)
     (reduce *))
;; 2122848000!

;; ---------- ---------- ---------- ---------- ----------

;; part 2 리팩토링

;; 지도의 x,y좌표가 tree 인지 아닌지 반환
;; -> y 좌표를 명시적으로
(defn tree? [input]
  (fn [[x y]]
    (= \# (nth (input y) x))))
(def tree?-with-input (tree? day3-input))

(def ch-per-line 31)

(defn make-path [input]
  (let [height (count input)]
    (fn [[dx dy]]
      (loop [paths [], x dx, y dy]
        (if (< y height)
          (recur (conj paths (vector (mod x ch-per-line) y)) (+ x dx) (+ y dy))
          paths)))))
(def make-path-with-input (make-path day3-input))

;; dx dy 들
(def slopes [[1 1] [3 1] [5 1] [7 1] [1 2]])

; 한 경우에 대해 tree 개수 세기
(defn tree-count [*slope]
  (->> *slope
       (map tree?-with-input)
       (filter true?)
       (count)))

(tree-count (nth (map make-path-with-input slopes) 0))
; => 80

; 여러 경우를 count 하기
(reduce * (map tree-count (map make-path-with-input slopes)))

; solution
(->> slopes
     (map make-path-with-input)
     (map tree-count)
     (reduce *))

; (range 1 (count day3-input) 1)
;; range: start end step

;; make-path 함수를 range로 구현해보기
(defn make-path-with-range [input]
  (let [height (count input)]
    (fn [[dx dy]])))

(defn make-vector-with-range [rx ry]
  (map vector
       (map #(mod % 31) (range rx (count day3-input) rx))
       (range ry (count day3-input) ry)))

(make-vector-with-range 3 1)

(map vector
     (range 1 (count day3-input) 1)
     (range 3 (count day3-input) 3))

(range 3 (count day3-input) 3)
; (range dx (count day3-input) dx)
(range 1 (count day3-input) 1)
; (range dy (count day3-input) dy))

(defn make-path-old [input]
  (let [height (count input)]
    (fn [[dx dy]]
      (loop [paths [], x dx, y dy]
        (if (< y height)
          (recur (conj paths (vector (mod x ch-per-line) y)) (+ x dx) (+ y dy))
          paths)))))
(def make-path-input-and-range (make-path day3-input))