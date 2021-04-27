(ns day2
  (:require [clojure.java.io :as io]))

(def input (-> "day2.txt"
               (io/resource)
               (slurp)
               (clojure.string/split-lines)))

; part 1.
(defn frequency
  [v m]
  (get (frequencies m) v 0))

(defn valid-password? [{i :i j :j ch :ch password :password}]
  (let [freq (frequency ch password)]
    (<= i freq j)))

(def line-regex #"(\d+)-(\d+) (\w{1}): (\w+)")

(defn input-map [line]
  (let [[_ i j ch password] (re-find line-regex line)]
    {:i        (Integer/parseInt i)
     :j        (Integer/parseInt j)
     :ch       (first ch)
     :password password}))

; part 1 answer
(->> input
     (map input-map)
     (filter valid-password?)
     (count))

; part 2.
; 글자 두개 꺼내서 한 번만 있는지 확인
;   [{i :i j :j ch :ch password :password}] 이건
;== [{:keys [i j ch password]}] 와 같다.
(defn valid-password2? [{i :i j :j ch :ch password :password}]
  (let [freq (frequency ch [(get password (dec i)) (get password (dec j))])]
    (= freq 1)))

; part 2 answer
(->> input
     (map input-map)
     (filter valid-password2?)
     (count))

; ----------
(comment
  (get-two-ch 1 2 "abcde")
  (pr input)
  (frequencies "xpxc")
  (frequency \x "xpxc")
  (valid-password? {:i "3" :j "5" :ch "v" :password "qvjvjdhvl"})
  (parse-input "1-2 x: xpxc")
  (input-map ["1-2 x: xpxc"])
  (first "x")
  (re-find (re-matcher line-regex "1-2 x: xpxc"))
  (->> ["1-2 x: xpxc" "1-12 x: xpxc"]
       (map input-map)
       (map valid-password?)))
