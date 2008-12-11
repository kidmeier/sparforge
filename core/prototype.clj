(ns core.prototype
  (:require [clojure.zip :as z]))

;; Node management
(defmacro defproto 
  "Define a new node sub-prototype with the given name based on the given prototype.
   The new node type is registered with in an isa relationship with the prototype."
  [name parent & keyvals]
  
  (let [properties (apply #'hash-map keyvals),
	sym (symbol (str (.name *ns*)) (str name)),
	def-form  `(def ~name 
			(struct-map (create-struct :prototype 
						   :symbol
						   ~@(map #'first properties))
			  :prototype ~parent,
			  :symbol (symbol (str (.name *ns*)) (str '~name)),
			  ~@keyvals))]

    ;; If we have a prototype, we emit the def-form, plus we also create an isa
    ;; relationship between the derived node and the prototype; we still return
    ;; the var to be symmetric with the no prototype path.
    (if parent
      `(do ~def-form
	   (derive (:symbol ~name) (:symbol ~parent))
	   (var ~name))
      ;; Just the def
      def-form)))

(defn clone
  "Clone and extend the node with the supplied keyvals."
  [node & keyvals]

  (apply #'assoc :prototype node keyvals))

;; Property access ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn has-property
  "Test if the given property is present in the node, or any of its prototypes."
  [node prop]

  (or (contains? node prop)
      (when (:prototype node)
	(has-property (:prototype node) prop))))

(defn lookup
  "Retrieve the property 'prop in 'node, if the property is not found in 'node, then
   it is searched for in the node's :prototype (if any)."
  [node prop]

  (if (contains? node prop)
    (node prop)

    (if (:prototype node)
      (recur (:prototype node) prop)
      nil)))

;; Traversal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- node-zip 
  [node]

  (z/zipper 
   (fn [node] (:children node)) 
   (fn [node] (:children node)) 
   (fn [node children]
     (assoc node :children children))
   node))

(defn traverse 
  "Traverse the graph creating returning the new graph as updated (if at all) by the
   traversal function f.

   The arguments to the traversal function are:
     loc - a zipper location
     node - the node at the location 'loc
     & args - any arguments passed into traverse

   The traversal function should return a vector whose first element is the modified
   location (if at all) followed by the arguments to be passed into 'f on the next
   iteration."
  [root f & args]

  (loop [loc (node-zip root), 
	 args args]
    (if (z/end? loc)
      (z/root loc)
      (let [[new-loc & new-args] (apply f loc (z/node loc) args)]	
	(recur (z/next new-loc) new-args)))))

(comment

  (defn handle-event [loc node ev]
    
    (when (not (:handled ev))
      (if-let hndlr ((:name ev) node)
	      [loc (hndlr node ev)]
	      
	      [loc ev])))
  
  (defn key-handler [node ev]
    (println "Got keypress: " (:key ev))
    (assoc ev :handled true))
  )
  
;; Basic node prototypes

(defproto leaf nil)
(defproto container nil :children #{})
       
(comment
  (def *sg* 
       (clone
	container
	:children 
	[(clone leaf :value "First child"),
	 (clone leaf :value "Second child"),
	 (clone container,
		:children 
		[(clone leaf :value "First grandchild"),
		 (clone leaf :value "Second grandchild")]),
	 (clone leaf :value "Fourth child")]))
  )
