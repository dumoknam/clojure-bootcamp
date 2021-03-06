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

;; ---------- ---------- ---------- ---------- ---------- ----------
;; part 1. refactoring
;; {:row :column} 을 컬렉션 대신 하나의 데이터로 취급하도록 변경
(defn seat-map [seat-str]
  {:row    (subs seat-str 0 7)
   :column (subs seat-str 7 10)})

(defn binary-match [seat-ch]
  (case seat-ch
    \F 0
    \L 0
    \B 1
    \R 1
    0))

(defn parse-int [binary-list]
  (-> binary-list
      (clojure.string/join)
      (Integer/parseInt 2)))

(defn seat-to-integer-map [seat-map]
  (into {} (for [[k v] seat-map]
             [k (->> v
                     (map binary-match)
                     (parse-int))])))

(defn calculate-id [{row :row column :column}]
  (+ (* row 8) column))

(defn ->seat-id [seat]
  (->> seat
       (seat-map)
       (seat-to-integer-map)
       (calculate-id)))

; part 1 solution.
(apply max (map ->seat-id input))

(comment
  (calculate-id (seat-to-integer (seat-map "FBFBBFFRLL"))))

;; ---------- ---------- ---------- ---------- ---------- ----------
;; part 2.
(def seat-ids (map ->seat-id input))
(def max-id (apply max seat-ids))
(def min-id (apply min seat-ids))

; min 부터 high 까지의 합 - 있는 id들의 합 = 없는 id값 (answer)
(-
  (apply + (range min-id (+ 1 max-id)))
  (apply + seat-ids))

(comment
  (prn max-id min-id))
