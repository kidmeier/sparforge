(ns ui.node.util
  (:use core.prototype)
  (:require [ui.scene :as scene]))

;; Clear screen ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defproto clear thing
  :color [0 0 0 0])

(defmethod scene/render (proto clear)
  [self]

  )