(ns day4
  (:require [clojure.java.io :as io]
            [clojure.string :as c-str]))

(def input (-> "day4.txt"
               (io/resource)
               (slurp)
               (clojure.string/split-lines)))

; "cid" = optional
(def required-keys #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

; 문제의 key < 필수 key 체크
; part 1.
(defn parse-input [inputs]
  (loop [result [], element "", lines inputs]
    (if (empty? lines)
      (conj result element)
      (if (= "" (first lines))
        (recur (conj result element) "" (rest lines))
        (recur result (clojure.string/join " " [element (first lines)]) (rest lines))))))

(defn split-by-colon [[lines]]
  (c-str/split lines #":"))

(defn split-by-space [line]
  (c-str/split line #"\s"))

(defn get-keys [line]
  (->> line
       (clojure.string/trim)
       (split-by-space)
       (map vector)
       (map split-by-colon)
       (map first)))

(defn intersection-with-required-keys [target]
  (clojure.set/intersection (set target) required-keys))

; part 1 solution.
(->> input
     (parse-input)
     (map get-keys)
     (map intersection-with-required-keys)
     (filter #(= required-keys %))
     (count))

(comment
  (pr required-keys)
  (clojure.string/split "hgt:189cm" #":")
  (clojure.string/split "hgt:189cm byr:1987 pid:572028668 iyr:2014 hcl:#623a2f" #" ")
  (clojure.set/difference #{"hi" "hi-hi"} (vec '("hi"))))
