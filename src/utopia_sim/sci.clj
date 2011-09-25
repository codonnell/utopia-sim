(ns utopia-sim.sci
  (:use utopia-sim.util)
  (:use (clojure.contrib [math :only [sqrt]]))
  (:use (clojure [string :only [join]])))

(def sci-effect-table
  {:income 1.4
   :build-eff 1.0
   :population 0.65
   :food 8.0
   :gains 1.4
   :thievery 6.0
   :magic 6.0})

(def sci-rate-table ;'(books-per-acre book-cost)
  {:none '(0 0)
   :minimal '(0.3 6)
   :limited '(0.4 7)
   :sustained '(0.5 9)
   :active '(0.7 13)
   :focused '(0.9 17)
   :accelerated '(1.2 22)
   :intensive '(1.5 27)
   :rushed '(2.0 36)
   :extreme '(3.0 50)})

(defn sci-cost [prov]
  (* (second ((:sci-rate prov) sci-rate-table))
     (- 1 (* 1.5
             (/ (:schools (:buildings prov)) (:acres prov))
             (- 1 (/ (:schools (:buildings prov)) (:acres prov)))
             (:build-eff prov)))
     (if-mult (= :sage (:personality prov)) 0.7)))

(defn sci-effect [prov sci-type]
  (+ 1
     (* (if-mult (= :undead (:race prov)) 0.85)
        (if-mult (= :sage (:personality prov)) 1.3)
        (sci-type sci-effect-table)
        (sqrt (/ (sci-type (:science prov)) (:acres prov)))
        (inc (* 2 (/ (:libraries (:buildings prov)) (:acres prov)))))))
