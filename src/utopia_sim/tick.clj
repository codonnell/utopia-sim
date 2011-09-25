(ns utopia-sim.tick
  (:use utopia-sim.econ)
  (:use utopia-sim.sci)
  (:use utopia-sim.mil)
  (:use utopia-sim.building)
  (:use utopia-sim.util)
  (:use [clojure.contrib.math :only [round]]))

(defn update-gold-soldiers [prov]
  (let [income (round (mod-income prov))
        wages (round (mil-expenses prov))
        science (round (sci-cost prov))
        [sol-cost rate] ((:draft-rate prov) draft-table)
        sols-drafted (min (round (* rate 
                                    (if-mult (:patriotism (:spells prov)) 1.3)
                                    (:peasants prov)))
                          (- (round (* (:draft-target prov) (cur-pop prov)))
                             (mil-pop prov)))]
    (-> prov
      (assoc :gold (- (+ (:gold prov) income)
                      wages science (round (* sols-drafted sol-cost))))
      (assoc :peasants (- (:peasants prov) sols-drafted))
      (update-in [:military :soldiers] #(+ % sols-drafted)))))

(defn update-food [prov]
  (assoc prov :food
         (- (+ (:food prov) (mod-food-production prov))
            (food-consumption prov)
            (food-decay prov))))

(defn update-runes [prov]
  (assoc prov :runes
         (- (+ (:runes prov) (rune-production prov))
            (rune-decay prov))))

(defn update-wizards [prov]
  (let [new-wizards (round (effect prov :guilds 0))]
    (-> prov
      (update-in [:peasants] #(- % new-wizards))
      (update-in [:wizards] #(+ % new-wizards)))))

(defn update-peasants [prov]
  (assoc prov :peasants
         (min (- (max-pop prov) (mil-pop prov) (:wizards prov))
              (round (* (:peasants prov) (inc (birth-rate prov)))))))

(defn completed-queue-items [coll]
  (zipmap (keys coll) (map first (vals coll))))

(defn update-queues [coll]
  (zipmap (keys coll) (map #(conj (subvec % 1) 0) (vals coll))))

(defn update-progress [prov elt]
  (let [completed (completed-queue-items (:in-progress (elt prov)))]
    (update-in prov [elt]
               #(assoc (merge-with + % completed)
                       :in-progress (update-queues (:in-progress %))))))

(defn update-buildings [prov]
  (let [completed (completed-queue-items (:in-progress (:buildings prov)))]
    (-> prov
      (update-in [:buildings]
               #(assoc (merge-with + % completed)
                       :in-progress (update-queues (:in-progress %))))
      (update-in [:acres] #(+ % (:barren completed))))))

(defn update-military [prov]
  (update-progress prov :military))

(defn update-science [prov]
  (let [completed
        (zipmap (keys (completed-queue-items (:in-progress (:science prov))))
                (map #(* %
                         (if-mult 
                           (:fountain-of-knowledge (:spells prov)) 1.1))
                     (vals (completed-queue-items 
                             (:in-progress (:science prov))))))]
    (update-in prov [:science]
               #(assoc (merge-with + % completed)
                       :in-progress (update-queues (:in-progress %))))))

(defn update-spells [prov]
  (let [cur-spells (remove (fn [[_ dur]] (if (= 0 dur) true false)) 
                           (:spells prov))]
    (assoc prov :spells
           (apply hash-map 
                  (apply concat 
                         (map (fn [[s dur]] 
                                [s (if (pos? dur) (dec dur) dur)])
                              cur-spells))))))

(defn update-protection [prov]
  (if (and (:protection prov) (pos? (:protection prov)))
    (update-in prov [:protection] dec)
    prov))

; Currently ignoring mil-eff and build-eff updating
(defn tick [prov]
  (-> prov
    update-buildings
    update-science
    update-peasants
    update-military
    update-food
    update-runes
    update-gold-soldiers
    update-spells
    update-protection))
