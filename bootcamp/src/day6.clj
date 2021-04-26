(ns day6
  (:require [clojure.java.io :as io]))

(def input (-> "day6.txt"
               (io/resource)
               (slurp)
               (clojure.string/split-lines)))

; part 1.
; 빈줄이 나오면 새로운 벡터로 저장
(defn parse-input [inputs]
  (loop [result [], element [], lines inputs]
    (if (empty? lines)
      (conj result element)
      (if (= "" (first lines))
        (recur (conj result element) [] (rest lines))
        (recur result (conj element (first lines)) (rest lines))))))

; 글자 세기
(defn word-count [f word-strings]
  (->> word-strings
       (map set)
       (apply f)
       (count)))

; 합집합
(defn union-word-count [word-strings]
  (word-count clojure.set/union word-strings))

; answer
(->> input
     (parse-input)
     (map union-word-count)
     (apply +))

; part 2.
; 교집합
(defn intersection-word-count [word-strings]
  (word-count clojure.set/intersection word-strings))

; answer
(->> input
     (parse-input)
     (map intersection-word-count)
     (apply +))

(comment
  (map empty? input)
  (parse-input input)
  (union-word-count ["jrupzdlhixcbnkte" "inchptudjzxelrk" "dhiegazkptvqxmrucnj"]))