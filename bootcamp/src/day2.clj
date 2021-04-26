(ns day2
  (:require [clojure.java.io :as io]))

(def input (-> "day2.txt"
               (io/resource)
               (slurp)
               (clojure.string/split-lines)))

; part 1.
(defn frequency-ch-in-str [ch str]
  (get (frequencies str) (first ch) 0))

(defn valid-password? [{i :i j :j ch :ch password :password}]
  (and (<= (frequency-ch-in-str ch (seq password)) (Integer/parseInt j))
       (>= (frequency-ch-in-str ch (seq password)) (Integer/parseInt i))))

(def line-regex #"(\d+)-(\d+) (\w{1}): (\w+)")

(defn input-map [line]
  (zipmap [:i :j :ch :password]
          (rest (re-find line-regex line))))

; part 1 answer
(->> input
     (map input-map)
     (map valid-password?)
     (filter true?)
     (count))

; part 2.
; 글자 두개 꺼내서 한 번만 있는지 확인
(defn character-frequencies [{i :i j :j ch :ch password :password}]
  (frequency-ch-in-str
    ch
    [(get password (dec (Integer/parseInt i))) (get password (dec (Integer/parseInt j)))]))

; part 2 answer
(->> input
     (map input-map)
     (map character-frequencies)
     (filter #(= 1 %))
     (count))

; ----------
(comment
  (get-two-ch 1 2 "abcde")
  (pr input)
  (frequencies (seq "xpxc"))
  (frequency-ch-in-str \x "xpxc")
  (valid-password? {:i "3" :j "5" :ch "v" :password "qvjvjdhvl"})
  (parse-input "1-2 x: xpxc")
  (input-map ["1-2 x: xpxc"])
  (first "x")
  (re-find (re-matcher line-regex "1-2 x: xpxc"))
  (->> ["1-2 x: xpxc" "1-12 x: xpxc"]
       (map input-map)
       (map valid-password?)))
