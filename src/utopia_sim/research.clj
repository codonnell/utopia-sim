(ns utopia-sim.research
  (:use [clojure.contrib.math :only [round expt floor]]
        [utopia-sim.util])
  (:require [utopia-sim.building :as b]
            [utopia-sim.sci :as s]))

(defn research [prov b n]
  (let [n (min n (:books (:science prov)))]
    (-> prov
      (update-in [:science :books] #(- % n))
      (update-in [:science :in-progress b]
                 #(spread % n)))))
