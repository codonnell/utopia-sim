(ns utopia-sim.thievery
  (:use utopia-sim.sci)
  (:use utopia-sim.building)
  (:use (clojure.contrib [math :only [floor round]]))
  (:use (clojure [string :only [join]])))

(defn if-mult [p m]
  (if p m 1))

(defn mod-tpa [prov]
  (* (/ (:thieves prov) (:acres prov))
     (if-mult (:invisibility (:spells prov)) 1.1)
     (sci-effect prov :thievery)
     (cond (= :halfling (:race prov)) 1.4
           (= :human (:race prov)) 1.2
           (= :orc (:race prov)) 0.8
           (= :dwarf (:race prov)) 0.8
           :else 1)
     (effect prov :tds 1)))

(defn opt-thieves [res max-pct gains-per-thief]
  (/ (* res max-pct) gains-p-thief))

(defn yield [thieves rel-size gains-per-thief sci res-lost]
  (* thieves rel-size gains-per-thief sci
     (- 1 res-lost)))
