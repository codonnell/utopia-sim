(ns utopia-sim.econ
  (:use utopia-sim.util)
  (:use utopia-sim.sci)
  (:use utopia-sim.mil)
  (:use utopia-sim.building)
  (:require [utopia-sim.honor :as h])
  (:use (clojure.contrib [math :only [floor round]]))
  (:use (clojure [string :only [join]])))

(defn raw-income [prov]
  (+ (* 2.75 (min (jobs prov) (:peasants prov)))
     (/ (* 2.75 (max 0 (- (:peasants prov) (jobs prov)))) 3)
     (* 0.5 (:prisoners prov))
     (effect prov :banks 0)))

(defn mod-income [prov]
  (* (raw-income prov)
     (if-mult (:plague (:spells prov)) 0.85)
     (if-mult (:riots (:spells prov)) 0.8)
     (effect prov :banks 1)
     (sci-effect prov :income)
     (h/honor-effect prov :income)
     (if-mult (= :human (:race prov)) 1.3)
     (if-mult (= :merchant (:personality prov)) 1.3)
     (if-mult (:monarch prov) 1.1)
     (if-mult (:dragon prov) 0.9)))

(defn mil-expenses [prov]
  (* 0.5
     (reduce + 0 (map (:military prov) '(:def-specs :off-specs :elites)))
     (:wages prov)
     (effect prov :armouries 1)
     (if-mult (:inspire-army (:spells prov)) 0.85)
     (if-mult (:greed (:spells prov)) 1.25)
     (if-mult (= :aggressive (:stance prov)) 1.15)))

(defn raw-pop [prov]
  (+ (* 25 (- (:acres prov) (:barren (:buildings prov))))
     (* 15 (:barren (:buildings prov)))))

(defn max-pop [prov]
  (* (+ (* (raw-pop prov) (cond (= :faery (:race prov)) 0.9
                                (= :halfing (:race prov)) 1.05
                                :else 1))
        (* 8 (:homes (:buildings prov))))
     (sci-effect prov :population)
     (h/honor-effect prov :population)))

(defn cur-pop [prov]
  (+ (:peasants prov)
     (reduce + 0 (vals (dissoc (:military prov) :in-progress)))
     (reduce + (map #(reduce + %) (vals (:in-progress (:military prov)))))
     (:wizards prov)))

(defn birth-rate [prov]
  (* 0.0205
     (effect prov :homes 1)
     (if-mult (= :avian (:race prov)) 0.8)))

(defn networth [prov]
  (+ (* 15 (:acres prov))
     (* 40 (reduce + 0 (vals (dissoc (:buildings prov) :in-progress))))
     (* 15 (reduce + 0 (map #(reduce + 0 %) 
                            (vals (:in-progress (:buildings prov))))))
     (:peasants prov)
     (* 1.5 (:soldiers (:military prov)))
     (* (if (= :elf (:race prov)) 6 5) (:def-specs (:military prov)))
     (* (if (= :human (:race prov)) 3.2 4) (:off-specs (:military prov)))
     (* (last ((:race prov) elite-table)) (:elites (:military prov)))
     (* 4 (:thieves (:military prov)))
     (* 4 (:wizards prov))
     (* 0.6 (:horses prov))
     (/ (:gold prov) 1000)
     (/ (reduce + 0 (vals (dissoc (:science prov) :in-progress))) 92)))

(defn raw-food-production [prov]
  (+ (effect prov :farms 0)
     (effect prov :barren 1)
     (if (= :shepherd (:personality prov)) (* (:acres prov) 4) 0)))

(defn mod-food-production [prov]
  (* (raw-food-production prov)
     (sci-effect prov :food)
     (if-mult (:monarch prov) 1.1)
     (if-mult (:fertile-lands (:spells prov)) 1.25)
     (if-mult (:drought (:spells prov)) 0.75)))

(defn food-consumption [prov]
  (round (/ (cur-pop prov) 4)))

(defn food-decay [prov]
  (round (* (/ (:food prov) 100)
            (if-mult (:vermin (:spells prov)) 6))))

(defn rune-production [prov]
  (* (effect prov :towers 0) (sci-effect prov :magic)))

(defn rune-decay [prov]
  (round (* (:runes prov) 0.0118)))
