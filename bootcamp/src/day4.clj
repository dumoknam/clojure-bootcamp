(ns day4
  (:require [clojure.string :as c-str]
            [clojure.set :as c-set]))

(def input (util/file->lines "day4.txt"))

; 문제의 key < 필수 key 체크
; part 1.
; :cid = optional
(def required-keys #{:byr :iyr :eyr :hgt :hcl :ecl :pid})

(defn parse-input [inputs]
  (loop [result [], element "", lines inputs]
    (if (empty? lines)
      (conj result element)
      (if (= "" (first lines))
        (recur (conj result element) "" (rest lines))
        (recur result (if (empty? element)
                        (first lines)
                        (str element " " (first lines)))
               (rest lines))))))

(defn split-by-colon [[words]]
  (c-str/split words #":"))

(defn to-kv [kv]
  {(keyword (first kv)) (second kv)})

(defn into-map [kvs]
  (into {} (map to-kv kvs)))

; 파싱을 끝까지! map 으로 만들자!
(defn make-map [line]
  (->> line
       (#(c-str/split % #"\s"))
       (map vector)
       (map split-by-colon)
       (into-map)))

(defn input->map [input]
  (->> input
       (parse-input)
       (map make-map)))

(defn has-all-required-keys? [target]
  (let [diff (c-set/difference required-keys (set (keys target)))]
    (empty? diff)))

; part 1 solution.
(->> input
     (input->map)
     (filter has-all-required-keys?)
     (count))

; part 2.
; <= 1920 byr 2002
; <= 2010 iyr 2020
; <= 2020 eyr 2030
; hgt: <= 150 "cm" 193 or <= 59 "in" 76
; hcl: #[0-9a-f]{6}
; ecl: [amb blu brn gry grn hzl oth]
; pid: \d{9}

; 파싱 함수 추가
(defn parse-passport [{:keys [byr iyr eyr hgt hcl ecl pid]}]
  {:byr (Integer/parseInt byr)
   :iyr (Integer/parseInt iyr)
   :eyr (Integer/parseInt eyr)
   :hgt (let [[v unit] (rest (re-matches #"(\d+)(\w+)" hgt))]
          [(Integer/parseInt v) unit])                      ; [값 단위(cm/in)]
   :hcl hcl
   :ecl ecl
   :pid pid})

; 키 별 조건 검사
(defn valid-passport? [{:keys [byr iyr eyr hgt hcl ecl pid]}]
  (and (<= 1920 byr 2002)
       (<= 2010 iyr 2020)
       (<= 2020 eyr 2030)
       (case (second hgt)
         "in" (<= 59 (first hgt) 76)
         "cm" (<= 150 (first hgt) 193)
         false)
       (re-matches #"#[0-9a-f]{6}" hcl)
       (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)
       (re-matches #"\d{9}" pid)))

(->> input
     (input->map)
     (filter has-all-required-keys?)
     (map parse-passport)
     (filter valid-passport?)
     (count))

(comment
  (pr required-keys)
  (= required-keys #{:byr :iyr :eyr :hgt :hcl :ecl :pid})
  (->> input (parse-input))
  (c-str/join " " (remove c-str/blank? ["" "hello" "world"]))
  (valid-passport? {:hgt "173cm", :byr "1925", :pid "070222017", :iyr "2013", :hcl "#ceb3a1", :ecl "gry", :eyr "2024"})
  (clojure.string/split "hgt:189cm" #":")
  (clojure.string/split "hgt:189cm byr:1987 pid:572028668 iyr:2014 hcl:#623a2f" #" ")
  (clojure.set/difference #{"hi" "hi-hi"} (vec '("hi"))))
