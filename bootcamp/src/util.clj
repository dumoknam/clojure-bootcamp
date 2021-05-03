(ns util
  (:require [clojure.java.io :as io]))

(defn file->lines [filename]
  (-> filename
      (io/resource)
      (slurp)
      (clojure.string/split-lines)))

(defn str->int [s]
  (Integer/parseInt s))

(defn file->int-lines [filename]
  (->> filename
       (file->lines)
       (map str->int)))
