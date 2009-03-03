(ns ui.widget
  (:use core.prototype))

;; Base prototype for all UI widgets
(defproto widget container
  ;; Basic widget properties
  :size [::default ::default],
  :position [::default ::default])

(defproto panel widget)

(defmethod render ui.widget/)
