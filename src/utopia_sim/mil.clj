(ns utopia-sim.mil
  (:use utopia-sim.util)
  (:use utopia-sim.building)
  (:require [utopia-sim.honor :as h])
  (:use (clojure.contrib [math :only [floor round]]))
  (:use (clojure [string :only [join]])))

(def elite-table ; '(off def cost networth)
  {:avian '(8 3 650 6.5)
   :dwarf '(7 4 800 6.0)
   :elf '(7 3 750 6.0)
   :faery '(4 7 900 7.0)
   :halfling '(6 5 500 5.0)
   :human '(8 3 800 6.5)
   :orc '(9 2 800 6.75)
   :undead '(9 3 nil 7.0)})

(def draft-table ; '(cost-per-sol pct-of-peasants)
  {:reservist '(30 0.003)
   :normal '(50 0.006)
   :aggressive '(75 0.01)
   :emergency '(110 0.015)})

(defn draft-cost [prov]
  (* (first ((:draft-rate prov) draft-table))
     (effect prov :armouries 0)
     (if-mult (= :orc (:race prov)) 0.7)))

(defmulti train-cost (fn [_ x _] x))

(defmethod train-cost :off-specs [prov _ fast?]
  (* 350 (effect prov :armouries 2)
     (if-mult fast? 2.0)))
(defmethod train-cost :def-specs [prov _ fast?]
  (* 350 (effect prov :armouries 2)
     (if-mult fast? 2.0)))

(defmethod train-cost :elites [prov _ fast?]
  (* (nth ((:race prov) elite-table) 2) (effect prov :armouries 2)
     (if-mult fast? 2.0)))

(defmethod train-cost :thieves [prov _ fast?]
  (* 500 (if-mult (= :halfling (:race prov)) 0.5)
     (if-mult fast? 2.0)))

(defn mil-eff [prov]
  (let [w (:wages prov)]
    (- (+ (* (/ 4 3) w w w)
          (* 22 w)
          84.4)
       (* 8 w w))))

(defn off-mil-eff [prov]
  (* (:mil-eff prov)
     (effect prov :tgs 0)
     (h/honor-effect prov :off-mil-eff)
     (if-mult (:fanaticism (:spells prov)) 1.05)))

(defn def-mil-eff [prov]
  (* (:mil-eff prov)
     (effect prov :forts 0)
     (if-mult (or (:minor-protection (:spells prov))
                  (:greater-protection (:spells prov)))
              1.05)
     (if-mult (:fanaticism (:spells prov)) 0.97)
     (if-mult (:plague (:spells prov)) 0.85)))

(defn raw-off [prov mercs]
  (let [mil (:military prov)
        elite-list ((:race prov) elite-table)]
    (+ (* (if (:aggression (:spells prov)) 2 1) (:soldiers mil))
       (* (if (= (:race prov) :human) 4 5) (:off-specs mil))
       (* (first elite-list) (:elites mil))
       (min (:horses prov) (+ (:soldiers mil) (:off-specs mil) (:elites mil)))
       (* 3 (min (+ mercs (:prisoners prov))
                 (/ (+ (:soldiers mil) (:off-specs mil) (:elites mil)) 5))))))

(defn mod-off [prov mercs gens]
  (* (raw-off prov mercs)
     (off-mil-eff prov)
     (inc (* 0.03 (dec gens)))
     (if-mult (= :fortified (:stance prov)) 0.85)))

(defn raw-def [prov]
  (let [mil (:military prov)
        elite-list ((:race prov) elite-table)]
    (+ (* (if (:aggression (:spells prov)) 0 1) (:soldiers mil))
       (* (if (= (:race prov) :elf) 6 5) (:def-specs mil))
       (* (second elite-list) (:elites mil))
       (* (if (:town-watch (:spells prov)) 0.25 0) (:peasants prov)))))

(defn mod-def [prov]
  (* (max (:acres prov) (* (raw-def prov) (def-mil-eff prov)))
     (if-mult (= :fortified (:stance prov)) 1.15)))

(defn mil-pop [prov]
  (+ (reduce + 0 (vals (dissoc (:military prov) :in-progress)))
     (reduce + 0 (map #(reduce + 0 %) 
                      (vals (:in-progress (:military prov)))))))
