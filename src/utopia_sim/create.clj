(ns utopia-sim.create
  (:use utopia-sim.util)
  (:require [utopia-sim.building :as b]
            [utopia-sim.sci :as s]
            [utopia-sim.econ :as e]))

(defn new-prov [race pers prov-name ruler-name gender monarch?]
  (let [raw-prov
        {:race race
         :personality pers
         :prov-name prov-name
         :ruler-name ruler-name
         :gender gender
         :acres 400
         :wizards 0
         :military {:off-specs 0
                    :def-specs 0
                    :elites 0
                    :soldiers 625
                    :thieves 0
                    :in-progress
                    (zipmap '(:off-specs :def-specs :elites :thieves)
                            (repeat empty-progress))}
         :buildings (merge
                      (zipmap (keys b/effects-table) (repeat 0))
                      {:banks 40
                       :homes 40
                       :farms 40
                       :barren 280
                       :in-progress
                       (zipmap (keys b/effects-table) 
                               (repeat empty-progress))})
         :science (assoc (zipmap (keys s/sci-effect-table) (repeat 0))
                         :in-progress (zipmap (keys s/sci-effect-table)
                                              (repeat empty-progress))
                         :books 0)
         :honor 0
         :spells {}
         :prisoners 0
         :food 50000
         :gold 300000
         :runes 0
         :build-eff 1.0
         :sci-rate :none
         :draft-rate :aggressive
         :draft-target 0.0
         :mil-eff 1.0
         :trade-bal 0
         :dragon nil
         :monarch monarch?
         :stance :normal
         :wages 1.0
         :horses 0
         :protection 72
         :peasants 0
         :explored 0}]
    (assoc raw-prov :peasants (- (e/max-pop raw-prov) (e/cur-pop raw-prov)))))
