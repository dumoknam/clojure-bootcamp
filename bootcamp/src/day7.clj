(ns day7)
; Handy Haversacks

(def input (util/file->lines "day7.txt"))

(defn make-keyword [word]
  (keyword (clojure.string/replace word #" " "-")))

(defn to-kv [list]
  (if (= nil list)
    {}
    {(make-keyword (list 2)) (Integer/parseInt (list 1))}))

(defn line->map [line]
  (let [[_ bag in] (re-find #"(.*) contain (.*)." line)]
    {(make-keyword (second (re-find #"(.*) bags" bag)))
     (into {} (map to-kv (map #(re-find #"\s*(\d+)\s(.*) bag\.*" %) (clojure.string/split in #","))))}))

(def input-map
  (->> input
       (map line->map)
       (into {})))
;--- parsing

(defn key-of-map-value [k] (keys (input-map k)))

(defn has-shiny-gold [root]
  (loop [step (key-of-map-value root)]
    (if (or (nil? step) (= [nil] step))
      false
      (if (contains? (set step) :shiny-gold)
        true
        (recur (vec (set (flatten (map keys (map input-map step))))))))))

; part 1.
(->> input-map
     (keys)
     (filter has-shiny-gold)
     (count))
;348

(comment
  (def parsed-map                                           ; 예시를 맵으로
    {:light-red    {:bright-white 1 :muted-yellow 2}
     :dark-orange  {:bright-white 3 :muted-yellow 4}
     :bright-white {:shiny-gold 1}
     :muted-yellow {:shiny-gold 2 :faded-blue 9}
     :shiny-gold   {:dark-olive 1 :vibrant-plum 2}
     :dark-olive   {:faded-blue 3 :dotted-black 4}
     :vibrant-plum {:faded-blue 5 :dotted-black 6}
     :faded-blue   {}
     :dotted-black {}})
  (def ts "plaid beige bags contain 4 posh fuchsia bags, 1 posh violet bag, 1 drab gray bag, 4 pale white bags.")
  (contains? #{:bw :my} :ew)
  (contains? (set '(1 2 3)) 4)
  (keys (into {} (map line->map input)))
  (to-kv ["4 posh fuchsia bag" "4" "posh fuchsia"])
  (prn parsed-map)
  (prn (parsed-map :light-red))
  (prn (:light-red parsed-map))
  (key-of-values :shiny-gold)
  (def vv '({:shiny-gold 1} {:shiny-gold 2, :faded-blue 9}))
  (aa :dotted-black)
  (key-of-map-value :faded-blue)
  (vec (set (flatten (map keys (map parsed-map (key-of-map-value :light-red))))))
  (map key-of-map-value (vec (set (flatten (map keys (map parsed-map (key-of-map-value :light-red)))))))
  (def m1 (map key-of-map-value (vec (set (flatten (map keys (map parsed-map (key-of-map-value :light-red))))))))
  (vec (set (flatten (map keys (map parsed-map (flatten m1))))))
  (def m2 (vec (set (flatten (map keys (map parsed-map (flatten m1)))))))
  (= [nil] (flatten [nil]))
  (vec (set (flatten (map keys (map parsed-map (flatten m2))))))
  (key-of-map-value :light-red)
  (map keys (map parsed-map (key-of-map-value :light-red)))
  (map keys vv)
  (def vvv '((:shiny-gold) (:shiny-gold :faded-blue)))
  (def tmp-input
    ["bright indigo bags contain 4 shiny turquoise bags, 3 wavy yellow bags."
     "dotted turquoise bags contain 3 vibrant salmon bags, 2 dotted maroon bags, 1 bright beige bag, 1 drab white bag."
     "vibrant fuchsia bags contain 4 dark salmon bags."])
  (pr tmp-input)
  (map line->map input)
  (into {} (map line->map input))
  (into {}
        [(line->map "posh white bags contain 1 vibrant aqua bag.")
         (line->map "bright indigo bags contain 4 shiny turquoise bags, 3 wavy yellow bags.")])
  (p1 "bright indigo bags contain 4 shiny turquoise bags, 3 wavy yellow bags.")
  (p2 "bright indigo bags")
  (p3 "4 shiny turquoise bags, 3 wavy yellow bags")
  (line->map "bright indigo bags contain 4 shiny turquoise bags, 3 wavy yellow bags.")
  ;"** bags contain no other bags." 인 경우 처리
  (map to-kv (map #(re-find #"\s*(\d+)\s(.*) bag\.*" %) ["no other bags."]))
  (map #(re-find #"\s*(\d+)\s(.*) bag\.*" %) ["no other bags."])
  (re-find #"(.*) contain (.*)." ts)
  (def parsing1 (re-find #"(.*) contain (.*)." ts))

  ; parsing-2. key word
  (let [[_ bag _] parsing1]
    (make-keyword (second (re-find #"(.*) bags" bag))))
  (defn p1 [line]
    (re-find #"(.*) contain (.*)." line))
  (defn p2 [key-str]
    (make-keyword (second (re-find #"(.*) bags" key-str))))
  (defn p3 [value-str]
    (map to-kv (map #(re-find #"\s*(\d+)\s(.*) bag\.*" %) (clojure.string/split value-str #",")))))

; # input parsing
;plaid beige bags contain 4 posh fuchsia bags, 1 posh violet bag, 1 drab gray bag, 4 pale white bags.
;
;=> 1. contain 으로 자르기
;
;plaid beige bags
;( contain )
;4 posh fuchsia bags, 1 posh violet bag, 1 drab gray bag, 4 pale white bags.
;
;=> 2. bags 앞의 글자를 키워드로
;
;:plaid-beige (bags)
;( contain )
;4 posh fuchsia bags, 1 posh violet bag, 1 drab gray bag, 4 pale white bags.
;
;=> 3. 뒤의 글자들은 , 로 자르기
;
;:plaid-beige (bags)
;( contain )
;["4 posh fuchsia bags" "1 posh violet bag" "1 drab gray bag" "4 pale white bags"]
;
;=> 4. 숫자 - 문자열 - bags 로 찾기
;
;:plaid-beige (bags)
;( contain )
;[["4" "posh fuchsia"] ["1" "posh violet"] ["1" "drab gray"] ["4" "pale white"]
;
; => 5. k-v 형태로 변경
;
; :plaid-beige {
;   {:posh-fuchsia 4 :posh-violet 1 :drab-gray 1 :pale-white 4}
; }
