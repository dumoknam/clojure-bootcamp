(ns y2018_d5
  (:require [clojure.string :as c-str]))

(def polymers (util/file->lines "y2018_d5.txt"))

; part 1.
(def polymer (first polymers))
(def reg-p
  #"aA|bB|cC|dD|eE|fF|gG|hH|iI|jJ|kK|lL|mM|nN|oO|pP|qQ|rR|sS|tT|uU|vV|wW|xX|yY|zZ|Aa|Bb|Cc|Dd|Ee|Ff|Gg|Hh|Ii|Jj|Kk|Ll|Mm|Nn|Oo|Pp|Qq|Rr|Ss|Tt|Uu|Vv|Ww|Xx|Yy|Zz")

(defn replace-p [s] (c-str/replace s reg-p ""))
(defn find? [s] (re-find reg-p s))

(defn answer [p]
  (->> p
       (iterate replace-p)
       (drop-while find?)
       (first)
       (count)))

(answer polymer)
;=> 10496

; part 2.
(def units [#"a|A" #"b|B" #"c|C" #"d|D" #"e|E" #"f|F" #"g|G" #"h|H" #"i|I" #"j|J" #"k|K" #"l|L" #"m|M" #"n|N" #"o|O" #"p|P" #"q|Q" #"r|R" #"s|S" #"t|T" #"u|U" #"v|V" #"w|W" #"x|X" #"y|Y" #"z|Z"])

(defn generate-p2-polymer [unit]
  (c-str/replace polymer unit ""))

(->> units
     (map generate-p2-polymer)
     (map answer)
     (apply min))
;=> 5774 (2 min)
