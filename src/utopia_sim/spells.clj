(ns utopia-sim.spells
  (:use [clojure.contrib.math :only [round expt floor]]
        [utopia-sim.util])
  (:require [utopia-sim.building :as b]
            [utopia-sim.sci :as s]
            [utopia-sim.econ :as e]))

(def raw-spell-table ; '(cost duration #{races})
  {:minor-protection (list 0.35 12 #{:all})
   :greater-protection (list 0.45 24 #{:human})
   :fog (list 0.6 12 #{:dwarf})
   :magic-shield (list 0.5 14 #{:all})
   :mystic-aura (list 0.5 nil #{:elf})
   :fertile-lands (list 0.5 16 #{:all})
   :natures-blessing (list 0.5 16 #{:all})
   :love-and-peace (list 0.7 14 #{:all})
   :tree-of-gold (list 0.8 nil #{})
   :quick-feet (list 0.8 nil #{:halfling :elf})
   :builders-boon (list 1.0 12 #{:all})
   :inspire-army (list 1.1 12 #{:all})
   :anonymity (list 1.3 nil #{:all})
   :invisibility (list 1.35 12 #{})
   :clear-sight (list 1.4 18 #{:elf :avian})
   :mages-fury (list 1.4 8 #{:undead})
   :war-spoils (list 1.45 4 #{:halfling :undead})
   :fanaticism (list 1.5 8 #{:orc})
   :fountain-of-knowledge (list 1.55 18 #{:elf})
   :town-watch (list 1.6 10 #{:halfling :avian})
   :aggression (list 1.65 12 #{:human :orc})
   :animate-dead (list 1.7 nil #{:dwarf})
   :reflect-magic (list 1.8 12 #{:elf})
   :shadowlist (list 1.9 nil #{:all})
   :patriotism (list 2.0 12 #{:all})
   :paradise (list 3.0 nil #{:all})})

(defn format-spell-table [[cost-mod len races]]
  (list cost-mod len
        (conj (if (:all races) 
                (conj (disj races :all) 
                      :human :halfling :dwarf :elf :avian :undead :orc) 
                races)
              :faery)))

(def spell-table
  (zipmap (keys raw-spell-table)
          (map format-spell-table (vals raw-spell-table))))

(defn rune-cost [prov s]
  (let [[cost-mod _ _] (spell-table s)]
    (floor (* (+ 680 (* 0.68 (:acres prov))) cost-mod))))

(defmulti spell-cast (fn [_ s] s))

(defmethod spell-cast :default [prov s]
  (let [[_ len _] (spell-table s)]
    (-> prov
      (assoc-in [:spells s]
                (if (nil? len) -1
                  (int (+ (rand-int (* 1.5 len)) (* 0.5 len)))))
      (update-in [:runes] #(- % (rune-cost prov s))))))

(defmethod spell-cast :paradise [prov _]
  (let [k (+ 3 (rand-int 5))]
    (-> prov
      (update-in [:acres] #(+ % k))
      (update-in [:buildings :barren] #(+ % k))
      (update-in [:runes] #(- % (rune-cost prov :paradise))))))

(defmethod spell-cast :tree-of-gold [prov _]
  (-> prov
    (update-in [:gold] #(+ % (round (* (+ 0.4 (rand 0.4)) 
                                       (e/mod-income prov)))))
    (update-in [:runes] #(- % (rune-cost prov :tree-of-gold)))))
