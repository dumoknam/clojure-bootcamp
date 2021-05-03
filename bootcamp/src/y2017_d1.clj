(ns y2017_d1)
(def input (first (util/file->lines "y2017_d1.txt")))
;--- Day 1: Inverse Captcha ---
; part 1. 연속으로 두번 나오면 더하기
; list is circular

; part 1.
(def same-as-next-index
  (for [x (range (count input))
        :let [y (mod (inc x) (count input))]
        :when (= (nth input x) (nth input y))]
    x))

(->> same-as-next-index
     (map #(Integer/parseInt (str (nth input %))))
     (reduce +))

(comment
  (pr same-index)
  (reduce #(+ (Integer/parseInt (nth input %))) same-index)
  (nth input 0))

; part 2. 절반뒤에 또 나오면 더하기
(def same-as-half-forward-index
  (let [half-count (/ (count input) 2)]
    (for [x (range half-count)
          :let [y (+ x half-count)]
          :when (= (nth input x) (nth input y))]
      x)))

(->> same-as-half-forward-index
     (map #(Integer/parseInt (str (nth input %))))
     (reduce +)
     (* 2))
