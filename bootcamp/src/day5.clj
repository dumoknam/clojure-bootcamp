(ns day5
  (:require [clojure.java.io :as io]))

;; part 1.
(def input (-> "day5.txt"
               (io/resource)
               (slurp)
               (clojure.string/split-lines)))

; 2진수 매핑
; F, L = 0
; B, R = 1
(defn binary-match [seat-ch]
  (case seat-ch
    \F 0
    \L 0
    \B 1
    \R 1
    0))

; 2진수 list -> integer
; (0 1 0 1) => "0101" => 5
(defn parse-int [binary-list]
  (-> binary-list
      (clojure.string/join)
      (Integer/parseInt 2)))

(defn binary-string [seat-str]
  (map binary-match seat-str))

(defn subs-seat [seat]
  [(subs seat 0 7) (subs seat 7 10)])

; row & column 으로 seat id 계산
(defn calculate-id [row column]
  (+ (* row 8) column))

; ->seat-id
(defn ->seat-id [seat]
  (->> seat
       (subs-seat)
       (map binary-string)
       (map parse-int)
       (apply calculate-id)))

; 다 계산해서 max 때리기
(apply max (map ->seat-id input))
; => 980

(comment
  (pr 2r0101100)                                            ; FBFBBFF
  (pr 2r101)                                                ; RLR
  (Integer/parseInt "0101100", 2)
  (parse-int '(0 1 0 1))
  ;=> 5
  (->seat-id "FBFBBFFRLL")
  (->seat-id "FBFBBFFRRR")
  (->seat-id "FBFBBFFRLR"))

;; part 2.
(def seat-ids (map ->seat-id input))
(def max-id (apply max seat-ids))
(def min-id (apply min seat-ids))

(defn sum-to [value]
  (apply + (range (+ 1 value))))

; 1부터 high 까지 합 - 1부터 (low-1) 까지 합 - 있는것들 = 없는 값
(apply - (sum-to max-id) (sum-to (- min-id 1)) seat-ids)
