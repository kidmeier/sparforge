(ns math.quat
  (:use math math.vector))

;; Quaternions
(defn quat
  "Create a quaternion with the given args."
  [w x y z]
  (with-meta [w,(vect x y z)] {:type ::quaternion}))

(defn unit-quat
  "Create a quaternion with a metadata map flagging the quaternion as a unit
   quaternion. The burden of proof is on the caller."
  [w x y z]
  (with-meta [w,(vect x y z)] {:type ::quaternion, :unit true}))

;; Normalization
(defmethod math/norm2 [::quaternion] [ [w [x y z]] ] (+ (* w w) (* x x) (* y y)))
(defmethod math/norm [::quaternion] [q] (Math/sqrt (norm2 q)))
(defmethod math/normalize [::quaternion] [ [w [x y z] :as q] ]
  (let [n (norm q)]
    (with-meta [(/ w n) [(/ x n) (/ y n) (/ z n)]] 
	       {:type ::quaternion,
		:unit true})))

;; Operators
(defmethod math/add [::quaternion ::quaternion]
  [[w1 [x1 y1 z1]]
   [w2 [x2 y2 z2]]]

  (quat (+ w1 w2) (+ x1 x2) (+ y1 y2) (+ z1 z2)))
    
(defmethod math/mul [::quaternion ::quaternion]
  [[w1 [x1 y1 z1] :as q1]
   [w2 [x2 y2 z2] :as q2]]
  
  (quat 
   (- (* w1 w2) (* x1 x2) (* y1 y2) (* z1 z2)),
   (- (+ (* w1 x2) (* x1 w2) (* y1 z2)) (* z1 y2)),
   (+ (- (* w1 y2) (* x1 z2)) (* y1 w2) (* z1 x2)),
   (- (+ (* w1 z2) (* x1 y2)) (+ (* y1 x2) (* z1 w2)))))

;; Conversions to other representations
(defn to-matrix
  "Return a flat column major matrix (suitable for use with OpenGL) that represents
   the transform encoded by this quaternion.
   Based on: http://www.flipcode.com/documents/matrfaq.html#Q54"
  [q]
  
  (let [[w [x y z]] (if (:unit ^q) q (normalize q)),
	w2 (* w w),
	x2 (* x x),
	y2 (* y y),
	z2 (* z z)]
    [(- 1 (* 2 y2) (* 2 z2)) (+ (* 2 x y) (* 2 w z)) (- (* 2 x z) (* 2 w y)) 0,
     (- (* 2 x y) (* 2 w z)) (- 1 (* 2 x2) (* 2 z2)) (+ (* 2 y z) (* 2 w x)) 0,
     (+ (* 2 x z) (* 2 w y)) (- (* 2 y z) (* 2 w x)) (- 1 (* 2 x2) (* 2 y2)) 0,
     0                       0                       0                       1]))

(defn from-matrix
  "Return a quaternion that is represented by the given rotation matrix.
   Based on: http://www.flipcode.com/documents/matrfaq.html#Q55"
  [m]

  (let [t (+ 1 (nth m ) (nth m 5) (nth m 10)),
	col [(fn [m]
	     (let [s (* 2 (Math/sqrt (- (+ 1.0 (nth m 0)) (nth m 5) (nth m 10))))
		   x (/ 0.5 s),
		   y (/ (+ (nth m 1) (nth m 4)) s),
		   z (/ (+ (nth m 2) (nth m 8)) s),
		   w (/ (+ (nth m 6) (nth m 9)) s)]
	       (quat w x y z))),
	     (fn [m]
	       (let [s (* 2 (Math/sqrt (- (+ 1.0 (nth m 5)) (nth m 0) (nth m 10))))
		     x (/ (+ (nth m 1) (nth m 4)) s),
		     y (/ 0.5 s),
		     z (/ (+ (nth m 6) (nth m 9)) s),
		     w (/ (+ (nth m 2) (nth m 8)) s)]
		 (quat w x y z))),
	     (fn [m]
	       (let [s (* 2 (Math/sqrt (- (+ 1.0 (nth m 10)) (nth m 0) (nth m 5))))
		     x (/ (+ (nth m 2) (nth m 8)) s),
		     y (/ (+ (nth m 6) (nth m 9)) s),
		     z (/ 0.5 s),
		     w (/ (+ (nth m 1) (nth m 4)) s)]
		 (quat w x y z)))]]

    (if (> t 0.0)
      (let [s (/ 0.5 (Math/sqrt t))]
	(quat (/ 0.25 s),
	      (* (- (nth m 9) (nth m 6)) s),
	      (* (- (nth m 2) (nth m 8)) s),
	      (* (- (nth m 4) (nth m 1)) s)))
      ;; Not so lucky, determine which column has the largest diagonal element 
      (let [[_ c] (reduce (fn [[mx ix] i] (if (> (nth m i) mx) [(nth m i) i] [mx ix])) 
			   [(nth m 0) 0] [5 10])]
	 ( (nth col c) m )))))

(defn from-axis-angle
  "Convert a rotation represented with a rotation axis and angle to a quaternion.
   Based on: http://www.flipcode.com/documents/matrfaq.html#Q56"
  [axis angle]

  (let [cosa (Math/cos (/ angle 2)),
	sina (Math/sin (/ angle 2))]
    (normalize 
     (quat cosa,
	   (* sina (nth axis 0)),
	   (* sina (nth axis 1)),
	   (* sina (nth axis 2))))))

(defn from-spherical
  "Convert a rotation in spherical coordinates (latitude, longitude, angle) to a quaternion.
   Based on: http://www.flipcode.com/documents/matrfaq.html#Q58"
  [lat long angle]

  (let [sina (Math/sin (/ angle 2)),
	cosa (Math/cos (/ angle 2)),
	sinla (Math/sin lat),
	cosla (Math/cos lat),
	sinlo (Math/sin long),
	coslo (Math/cos long)]
    (quat cosa,
	  (* sina cosla sinlo),
	  (* sina sinla),
	  (* sina sinla coslo))))

;; Multiplicative identity
(def identity (with-meta (quat 1 0 0 0) {:unit true}))
