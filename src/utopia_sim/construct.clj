(ns utopia-sim.construct
  (:use [clojure.contrib.math :only [round expt floor]]
        [utopia-sim.util])
  (:require [utopia-sim.building :as b]
            [utopia-sim.sci :as s]))

(defn construct-time [prov fast?]
  (round
    (* 16
       (if-mult (= :shepherd (:personality prov)) 0.7)
       (if-mult (:builders-boon (:spells prov)) 0.9)
       (if-mult (= :fortified (:stance prov)) 0.8)
       (cond (and fast? (:protection prov)) 0.75
             fast? 0.5
             :else 1))))

(defn construct-cost [prov fast?]
  (round 
    (* (/ 8 31)
       (+ (:acres prov) 2900)
       (if-mult (= :dwarf (:race prov)) 0)
       (if-mult (= :shepherd (:personality prov)) 0.7)
       (b/effect prov :mills 0)
       (if-mult fast? 2))))

(defn raze-cost [prov]
  (round (/ (+ 1400 (:acres prov)) 4)))

(defn construct [prov b n fast?]
  (let [t (construct-time prov fast?)
        c (construct-cost prov fast?)
        n (min (:barren (:buildings prov))
               (if (zero? c) (:barren (:buildings prov))
                 (/ (:gold prov) c))
               n)
        c (* n c)]
    (-> prov
      (update-in [:gold] #(- % c))
      (update-in [:buildings :barren] #(- % n))
      (update-in [:buildings :in-progress b]
                 #(assoc % (dec t) (+ n (get % (dec t))))))))

(defn raze [prov b n]
  (let [n (min (b (:buildings prov)) n)
        c (* n (raze-cost prov))]
    (-> prov
      (update-in [:gold] #(- % c))
      (update-in [:buildings b] #(- % n))
      (update-in [:buildings :barren] #(+ % n)))))

(defn raw-explore-gold-cost [prov] ; TODO: Figure this out exactly
  (+ 600 (* 2.3 (expt (* 0.7912 (:acres prov)) 1.03))))

(defn explore-gold-cost [prov]
  (round (* (raw-explore-gold-cost prov)
            (if-mult (= :shepherd (:personality prov)) 0.7)
            (b/effect prov :mills 1)
            (if-mult (or (= :war (:stance prov)) 
                         (= :fortified (:stance prov))) 
                     2.0))))

(defn raw-explore-soldier-cost [prov] ; TODO: Figure this out exactly
  (+ 3 (* 0.0035847 (expt (:acres prov) 1.03))))

(defn explore-soldier-cost [prov]
  (round (* (raw-explore-soldier-cost prov)
            (if-mult (= :shepherd (:personality prov)) 0.7)
            (if-mult (or (= :war (:stance prov)) 
                         (= :fortified (:stance prov))) 
                     2.0))))

(defn explore-cost [prov]
  [(explore-gold-cost prov) (explore-soldier-cost prov)])

(defn explore-time [prov]
  (* 24 (if-mult (= :shepherd (:personality prov)) 0.7)))

(defn explore [prov n]
  (let [t (explore-time prov)
        [gold-cost sol-cost] (explore-cost prov)
        n (min (floor (/ (:gold prov) gold-cost))
               (floor (/ (:soldiers (:military prov)) sol-cost))
               (/ (b/built-acres prov) 2)
               n)]
    (-> prov
      (update-in [:gold] #(- % (* n gold-cost)))
      (update-in [:military :soldiers] #(- % (* n sol-cost)))
      (update-in [:buildings :in-progress :barren]
                 #(vec (concat (spread (subvec % 0 t) n) (subvec % t)))))))

(defn exploreable [prov]
  (let [[gold-cost sol-cost] (explore-cost prov)
        built (b/built-acres prov)]
    (min (floor (/ (:gold prov) gold-cost))
         (floor (/ (:soldiers (:military prov)) sol-cost))
         (if (neg? (- built (:barren (:buildings prov)))) 
           0
           (- built (:barren (:buildings prov)))))))
