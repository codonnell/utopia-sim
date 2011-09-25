(ns utopia-sim.honor)

(def rank-table ; key = min honor - 1, val = multiplier
  (sorted-map-by >
    6999 '(11 "Prince" "Princess")
    5500 '(9 "Duke" "Duchess")
    4500 '(7 "Count" "Countess")
    3750 '(5 "Viscount" "Viscountess")
    3000 '(3 "Baron" "Baroness")
    2250 '(2 "Lord" "Noble Lady")
    1500 '(1 "Knight" "Lady")
    -1 '(0 "Peasant" "Peasant")))

(def effects-table
  {:income 0.02
   :population 0.02
   :off-mil-eff 0.02
   :magic 0.03
   :thievery 0.03})

(defn rank [prov]
  (loop [ranks rank-table]
    (if (> (:honor prov) (ffirst ranks)) 
      (if (:monarch prov) 
        (cons (first (second (first ranks))) '("King" "Queen"))
        (second (first ranks)))
      (recur (dissoc ranks (ffirst ranks))))))

(defn honor-effect [prov attr]
  (+ 1 (* (first (rank prov)) (effects-table attr))))
