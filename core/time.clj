(ns core.time)

(defn millis [] (System/currentTimeMillis))
(defn nanos [] (System/nanoTime))
