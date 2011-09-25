(ns utopia-sim.prov
  (:use utopia-sim.sci)
  (:use utopia-sim.building)
  (:use utopia-sim.util)
  (:use (clojure.contrib [math :only [floor round]]))
  (:use (clojure [string :only [join]])))

(def prov
  {:acres 1200
   :race :dwarf
   :personality :merchant
   :wizards 100
   :military {:off-specs 0
              :def-specs 5000
              :elites 0
              :soldiers 0
              :thieves 600
              :in-progress (zipmap '(:off-specs :def-specs :elites :thieves)
                                   (repeat empty-progress))}
   :buildings (merge (zipmap (keys effects-table) (repeat 0))
                     {:banks 180
                      :armouries 300
                      :mills 120
                      :farms 144
                      :homes 120
                      :guilds 132
                      :towers 84
                      :forts 120
                      :in-progress (zipmap (keys effects-table) 
                                           (repeat empty-progress))})
   :science {:income 0
             :build-eff 0
             :population 0
             :gains 0
             :food 0
             :thievery 0
             :magic 0
             :in-progress (zipmap (keys sci-effect-table)
                                  (repeat empty-progress))
             :books 0}
   :honor 800
   :spells {:inspire-army 5
            :minor-protection 5
            :patriotism 5}
   :prisoners 0
   :peasants 25260
   :food 50000
   :runes 10000
   :gold 200000
   :horses 0
   :build-eff 1.25
   :sci-rate :none
   :wages 1.0
   :draft-rate :aggressive
   :draft-target 0.5
   :mil-eff 1.0
   :trade-bal 0
   :dragon nil
   :monarch true
   :stance :normal
   :protection 0
   :explored 0})
