(ns math)

(defn- type
  [& xs]
  (vec (map #(or (:type ^%1)
		 (class %1)) xs)))

;; Generic methods on various mathematical types. 
;; For symmetry, default impls are given for java.lang.Number
(defmulti norm #'type)
(defmethod norm [Number] [x] x)

(defmulti norm2 #'type)
(defmethod norm2 [Number] [x] (* x x))

(defmulti normalize #'type)
(defmethod normalize [Number] [x] 1)

(defmulti mul #'type)
(defmethod mul [Number Number] [a b] (* a b))

(defmulti div #'type)
(defmethod div [Number Number] [a b] (/ a b))

(defmulti dot #'type)
(defmethod dot [Number Number] [a b] (* a b))

(defmulti cross #'type)
(defmethod cross [Number Number] [a b] 
  (throw 
   (new IllegalArgumentException (str #'cross " is not defined for arguments [" a ", " b "]"))))

(defmulti add #'type)
(defmethod add [Number Number] [a b] (+ a b))

(defmulti sub #'type)
(defmethod sub [Number Number] [a b] (- a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Infix expression parser. Allows evaluation of infix math expressions
(declare *input* parse)

(defmulti parse-factor #(class (first *input*)))
(defmethod parse-factor Number []
  (let [fact (first *input*)]
    (var-set #'*input* (rest *input*))
    fact))

(defmethod parse-factor clojure.lang.IPersistentList []
  (let [rest-input (rest *input*)]
    (var-set #'*input* (first *input*))
    (let [fact (parse)]
      (var-set #'*input* rest-input)
      fact)))

(defmethod parse-factor clojure.lang.Symbol []
  (let [sym (first *input*)
	rest-input (rest *input*)]
    (var-set #'*input* (rest *input*))
    (cond 
     (= '| sym)
      (do
	(var-set #'*input* (take-while (fn [x] (not= '| x)) *input*))
	(let [fact (parse)]
	  (var-set #'*input* (loop [l rest-input] 
			       (if (= '| (first l)) (rest l) (recur (rest l)))))
	  (list `normalize fact)))
      (= '|| sym)
       (do
	 (var-set #'*input* (take-while (fn [x] (not= '|| x)) *input*))
	 (let [fact (parse)]
	   (var-set #'*input* (loop [l rest-input]
				(if (= '|| (first l)) (rest l) (recur (rest l)))))
	   (list `norm fact)))
       :else sym)))

(let [ops {'* `mul, '/ `div, '. `dot, '& `cross}]
  (defn match-term []
    (ops (first *input*)))

  (defn parse-term []
    (let [factor (parse-factor)]
      (if-let [op (match-term)]
	(do 
	  (var-set #'*input* (rest *input*))
	  (list op factor (parse-term)))
	factor))))

(let [ops {'+ `add, '- `sub}]
  (defn match-sum []
    (ops (first *input*)))
  (defn parse-sum []
    (let [term (parse-term)]
      (if-let [op (match-sum)]
	(do
	  (var-set #'*input* (rest *input*))
	  (list op term (parse-sum)))
	term))))

(def parse parse-sum)
(defmacro expr
  [& expr] 

  (binding [*input* expr]
    (let [form (parse)]
      form)))
