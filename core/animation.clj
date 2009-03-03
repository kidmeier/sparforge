(ns core.animation
  (:use core.prototype)
  (:require [core.time :as time]))

(defn animate
  [canvas scene f t & args]

  (let [dt (- (time/millis) t),
	result (apply f @scene t dt args)]
    (if result
      (dosync
       (ref-set scene result)
       (apply send canvas animate scene f )))))
