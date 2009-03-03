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
