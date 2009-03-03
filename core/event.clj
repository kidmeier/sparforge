(ns core.event
  (:use core.prototype)
  (:import (java.awt.event InputEvent KeyEvent MouseEvent)))


(defn- dispatch-event [loc node ev]
  
  (when (not (:handled ev))
    (if-let [hndlr ((:name ev) node)]
      [loc (hndlr node ev)]
      [loc ev])))

(defn handle-event
  [scene ev ev-kw]

  (traverse scene
	    dispatch-event
	    (assoc (bean ev)
	      :name ev-kw)))

(defmacro #^{:private true} defevent 
  [ev]

  `(defn ~ev
     [scene# ev#]

     (handle-event scene# ev# ~(keyword (str ev)))))

;; Standard keyboard and mouse events
(defevent key-down)
(defevent key-up)
(defevent key-pressed)

(defevent mouse-clicked)
(defevent mouse-down)
(defevent mouse-up)
(defevent mouse-entered)
(defevent mouse-exited)
(defevent mouse-wheel-moved)

