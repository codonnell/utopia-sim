(ns utopia-sim.train
  (:use [clojure.contrib.math :only [round expt floor]]
        [utopia-sim.util])
  (:require [utopia-sim.building :as b]
            [utopia-sim.sci :as s]
            [utopia-sim.mil :as m]))

(defn train-time [prov fast?]
  (round
    (* 24
       (if-mult (= :tactician (:personality prov)) 0.7)
       (if-mult (:inspire-army (:spells prov)) 0.8)
       (if-mult (= :war (:stance prov)) 0.75)
       (if-mult fast? 0.5))))

(defn train [prov u n fast?]
  (let [t (train-time prov fast?)
        n (min (:soldiers (:military prov))
               (floor (/ (:gold prov) (m/train-cost prov u fast?)))
               n)
        c (* n (m/train-cost prov u fast?))]
    (-> prov
      (update-in [:gold] #(- % c))
      (update-in [:military :in-progress u]
                 #(vec (concat (spread (subvec % 0 t) n) (subvec % t)))))))

(defmulti release (fn [_ u _] (if (= u :soldiers) :peasants :soldiers)))

(defmethod release :peasants [prov u n]
  (let [n (min (u (:military prov)) n)]
    (-> prov
      (update-in [:military u] #(- % n))
      (update-in [:peasants] #(+ % n)))))

(defmethod release :soldiers [prov u n]
  (let [n (min (u (:military prov)) n)]
    (-> prov
      (update-in [:military u] #(- % n))
      (update-in [:military :soldiers] #(+ % n)))))
