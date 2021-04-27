(ns util
  (:require [clojure.java.io :as io]))

(defn file->lines [filename]
  (-> filename
      (io/resource)
      (slurp)
      (clojure.string/split-lines)))
