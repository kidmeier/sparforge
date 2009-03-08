(ns core.util)

(defmacro defstatefn 
  "Defines a side-effecting function with cached argument. When called, the
   body is executed only if the arguments differ from the previous invocation."
  [name & fdecl]

  `(let [state# (atom nil)]
     (defn ~name [& args#]
       (let [f# (fn ~@fdecl)]
	 (if (not= (deref state#) args#)
	   (do
	     (apply f# args#)
	     (swap! state# (constantly args#))))))))

(defn apply-properties [instance props]
  "Applies the given property map to 'instance' which is presumed to be a Java
   Object. Each property is converted in a setter method based on the JavaBean
   convention."
  (let [clazz (class instance),
	prims {Boolean Boolean/TYPE,
	       Byte Byte/TYPE,
	       Character Character/TYPE,
	       Double Double/TYPE,
	       Float Float/TYPE,
	       Integer Integer/TYPE,
	       Long Long/TYPE,
	       Short Short/TYPE,
	       Void Void/TYPE}]
    (doseq [[prop value] props]
      (let [property-name (str (.sym prop)),
	    method-name (str "set" (.toUpperCase (.substring property-name 0 1))
			     (.substring property-name 1)),
	    signature (into-array Class [(or (prims (class value)) (class value))]),
	    args (into-array Object [value])
	    m (.getMethod clazz method-name signature)]
	(.invoke m instance args)))))

(defn unbean-into [instance props]
  "Takes a class and a map of property name/values and returns an instance of 
   that class with those property values

   Based on contribution by Bill Smith: 
   http://groups.google.com/group/clojure/msg/cfda480105dd5edb"
 (let [clazz (class instance)
       pmap (reduce 
	     (fn [m pd]
	       (let [name (.getName pd)
		     method (.getWriteMethod pd)]
		 (if (and method (= 1 (alength (. method (getParameterTypes)))))
		   (assoc m (keyword name) 
			  (fn [v] (. method (invoke instance 
						    (into-array [v])))))
		   ;; else
		   m)))
	     {}
	     (.getPropertyDescriptors (java.beans.Introspector/getBeanInfo
				       clazz)))]
   (doseq [kv props]
     (((keyword (first kv)) pmap) (second kv)))
   instance))

(defn unbean [clazz props]
  "Creates a new instance of clazz and calls unbean-into on it with the 
   supplied props."
  (unbean-into (.newInstance clazz) props))

