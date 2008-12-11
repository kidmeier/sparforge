(ns game.codex
  (:use core.prototype))

(defproto model nil
  :name :none, 
  :race :none,   
  :points -1,    ; Points value
  :base :none,   ; Base of model, a shape 
  :height :none, ; Height class of model
  :type :none)   ; Infantry, Jump Infranty, etc.


;; Creatures are models with a collection of characteristics
(defproto creature model
  :ws 0, :bs 0, :s 0, :t 0, :w 0, :i 0, :a 0, :ld 0, :sv 0)

;; Vehicles are also models but with a different set of characteristics
(defproto vehicle model
  :fa 0, :sa 0, :ra 0, :bs 0)

(defproto race nil
  :name :none,
  :models #{},
  :armoury #{})

(defproto army nil
  :race :none, :units [])

(defmacro defrace
  [symbol name edition models armoury]

  `(def ~symbol
	(clone race
	       :name ~name,
	       :edition ~edition
	       :models (into #{} ~(map #(assoc %1 :race ~name) models)),
	       :armoury (into #{} ~(map #(assoc %1 :race ~name) armoury)))))

(comment
  (defrace space-marines "Space Marines" "4th edition"
    (org-chart ...)
    (infantry ...)
    (hq ...)
    (heavy ...)
    (vehicle ...)
    (weapon ...))
  )

(defn- make-creature
  [name points base height type ws bs s t i a ld sv]

  (clone creature
	 :name name, :points points, :base base, :height height, :type type,
	 :ws ws, :bs bs, :s s, :t t, :i i, :a a :ld ld :sv sv))

(defn infantry
  [name points base ws bs s t i a ld sv]
  (make-creature name points base :standard :infantry ws bs s t i a ld sv))

(defn jump-infantry
  [name points base ws bs s t i a ld sv]
  (make-creature name points base :standard :jump-infantry ws bs s t i a ld sv))

(defn beast
  [name points base ws bs s t i a ld sv]
  (make-creature name points base :large :beasts-cavalry ws bs s t i a ld sv))

(def cavalry beast)

(defn bike
  [name points base ws bs s t i a ld sv]
  (make-creature name points base :standard :bikes ws bs s t i a ld sv))

(defn monster
  [name points base ws bs s t i a ld sv]
  (make-creature name points base :large :monster ws bs s t i a ld sv))

(defn artillery
  [name points base ws bs s t i a ld sv]
  (make-creature name points base :standard :artillery ws bs s t i a ld sv))

(defn vehicle
  (name points base 
