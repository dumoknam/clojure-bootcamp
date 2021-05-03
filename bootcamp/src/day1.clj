(ns day1)
(def input (util/file->int-lines "day1.txt"))

(def input-set
  (->> input
       (apply sorted-set)))

(defn sum=value? [n]
  (fn [i]
    (let [target (- n i)]
      [(contains? input-set target) i])))

(def sum=2020? (sum=value? 2020))

(defn find-answer [f s]
  (->> s
       (map f)
       (filter #(true? (first %)))
       (map #(second %))))

; part 1. a + b = 2020
(->> input-set
     (find-answer sum=2020?)
     (reduce *))

; part 2. c + d + e = 2020
(defn sum=diff? [n]
  (let [f (sum=value? (- 2020 n))]
    (->> input-set
         (find-answer f))))

(->> input-set
     (map sum=diff?)
     (filter #(not (empty? %)))
     (flatten)
     (apply sorted-set)
     (reduce *))


;; refactoring 1. for - list comprehension
;=> part 1.
(apply * (first (for [x input-set
                      y input-set
                      :when (= 2020 (+ x y))]
                  [x y])))
;=> part 2.
(apply * (first (for [x input-set
                      y input-set
                      z input-set
                      :when (= 2020 (+ x y z))]
                  [x y z])))

