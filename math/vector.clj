(ns math.vector
  (:use math))

;; Vector math
(defn vect
  ([x y]
     (vect x y 0))
  ([x y z]
     (with-meta [x y z] {:type ::vector})))

(defn unit-vect
  ([x y]
     (with-meta [x y 0] {:type ::vector, :unit true}))
  ([x y z]
     (with-meta [x y z] {:type ::vector, :unit true})))

;; Normalization
(defmethod math/norm2 [::vector] [[x y z]] (+ (* x x) (* y y) (* z z)))
(defmethod math/norm [::vector] [v] (Math/sqrt (norm2 v)))
(defmethod math/normalize [::vector]
  [[x y z :as v]]
  (if (:unit ^v) 
    v 
    (let [l (norm v)]
      (with-meta [(/ x l) (/ y l) (/ z l)] {:type ::vector, :unit true}))))

;; Basic arithmetic
(defmethod math/add [::vector ::vector]
  [[x1 y1 z1]
   [x2 y2 z2]]
  (vect (+ x1 x2) (+ y1 y2) (+ z1 z2)))

(defmethod math/sub [::vector ::vector]
  [[x1 y1 z1]
   [x2 y2 z2]]
  (vect (- x1 x2) (- y1 y2) (- z1 z2)))

(defmethod math/mul [Number ::vector]
  [s [x y z]]
  (vect (* s x) (* s y) (* s z)))

(defmethod math/div [Number ::vector]
  [s [x y z]]

  (vect (/ x s) (/ y s) (/ z s)))

;; Dot and cross product
(defmethod math/dot [::vector ::vector] [v1 v2]
  (reduce #'+ (map #'* v1 v2)))

(defmethod math/cross [::vector ::vector]
  [[x1 y1 z1 :as v1]
   [x2 y2 z2 :as v2]]
  (with-meta 
   [(- (* y1 z2) (* z1 y2)),
    (- (* z1 x2) (* x1 z2)),
    (- (* x1 y2) (* y1 x2))]
   {:type ::vector
    :unit (and (:unit ^v1)
	       (:unit ^v2))}))
