(ns prim
 (:gen-class))

(defn square [#^floats x] (unchecked-multiply x x))

(defn square2 [x]
  (let [xx (float x)]
    (unchecked-multiply xx xx)))



