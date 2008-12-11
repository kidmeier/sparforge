(ns core.relvar)

(defstruct relvar-struct
  :columns,
  :indices,
  :row-seq)

(defn- map-indices [columns]
  (apply hash-map (interleave columns (iterate inc 0))))

(defmacro defrelvar 
  [name & columns]

  `(def ~name 
	(struct-map relvar-struct
	  :columns [~@columns],
	  :indices ~(map-indices columns),
	  :rows (ref #{})
	  :row-seq ~(fn [rel] (seq @(:rows rel))))))

(defn row-seq [rel]
  (seq ((:row-seq rel) rel)))

(defn insert 
  [relvar & values]
  
  (if (not= (count values) (count (:columns relvar)))
    (throw (new IllegalArgumentException 
		(str "arity mismatch: " (:columns relvar) " vs. " values)))
    (dosync
     (alter (:rows relvar) conj (vec values)))))

;; Relational operators
(defn restrict 
  [parent predicate]

  (struct-map relvar-struct
    :columns (:columns parent),
    :indices (:indices parent),
    :row-seq (fn [rel] 
	      (filter predicate (row-seq parent)))))

(defn project
  [parent & columns]
  
  (struct-map relvar-struct
    :columns (into [] columns),
    :indices (map-indices columns),
    :row-seq (fn [rel]
	       (let [prj-row (fn [row] 
			       (reduce (fn [ret col] 
					 (conj ret (nth row (col (:indices parent)))))
				       [] 
				       columns)),
		     prj-seq (fn prj-seq [rows]
			       (when rows
				 (lazy-cons (prj-row (first rows))
					    (prj-seq (rest rows)))))]
		 (prj-seq (row-seq parent))))))


(defn product
  [a b]

  (let [columns (concat (:columns a) (:columns b))]
    (struct-map relvar-struct
      :columns columns,
      :indices (map-indices columns),
      :row-seq (fn [rel]
		 (mapcat (fn [x]
			   (map (fn [xx] (concat x xx)) (row-seq b)))
			 (row-seq a))))))

(defn union 
  [a b] 

  (when (not= (:columns a) (:columns b))
    (throw (new IllegalArgumentException (str "Relations are not union compatible"))))

  (struct-map relvar-struct
    :columns (:columns a),
    :indices (:indices a),
    :row-seq (fn [rel]
	       (let [seen (ref (into #{} (row-seq a)))]		 
		 (lazy-cat (row-seq a)
			   (filter 
			    (fn [row] 
			      (if (not (contains? @seen row))
				(dosync
				  ;; Record this row
				  (alter seen conj row)
				  row))) 
			    (row-seq b)))))))
		 

(defn intersection
  [a b]

  (when (not= (:columns a) (:columns b))
    (throw (new IllegalArgumentException (str "Relations are not intersection compatible"))))

  (struct-map relvar-struct
    :columns (:columns a)
    :indices (:indices a),
    :row-seq (fn [rel]
	       (let [seen (into #{} (row-seq a))]
		 (filter (fn [row]
			   (if (contains? seen row)
			     row))
			 (row-seq b))))))

(defn difference 
  [a b]

  (when (not= (:columns a) (:columns b))
    (throw (new IllegalArgumentException (str "Relations are not difference compatible"))))

  (struct-map relvar-struct
    :columns (:columns a),
    :indices (:indices a),
    :row-seq (fn [rel]
	       (let [b-set (into #{} (row-seq b))]
		 (filter (fn [row]
			   (if (not (contains? b-set row))
			     row))
			 (row-seq a))))))

(defn join
  [a b]
  
  )

(defn divide
  [a b c]
  nil)

(comment
  (defrelvar employee :first :last :department)
  (insert employee "mike" "reid" "support")
  (insert employee "jasper" "chui" "support")

  (defrelvar department :department :unit)
  (insert department "support" "swg")
  (insert department "devl" "swg")
  (insert department "sales" "igs")
  (insert department "test" "swg")

  (defrelvar supplemental :first :last :department)
  (insert supplemental "russell" "howe" "support")
  (insert supplemental "frank" "qi" "support")

  (defrelvar address 
    :number
    :street,
    :city,
    :region,
    :country,
    :postal)
  )
