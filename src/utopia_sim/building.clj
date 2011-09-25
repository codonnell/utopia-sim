(ns utopia-sim.building
  (:use utopia-sim.sci)
  (:use (clojure.contrib [math :only [floor round]]))
  (:use (clojure [string :only [join]])))

(defn build-pprint [flat? n]
  (let [n (cond flat? n
                (< n 1) (* 100 (- 1 n))
                :else (* 100 (- n 1)))]
    (format "%1$.2f" n)))

; either '(:flat rate affected-by-be?) or
;        '(:pct rate max-rate affected-by-be?)
(def effects-table
  {:barren (list '(:flat 15 false)
                 '(:flat 2 false))
   :homes (list '(:flat 33 false)
                '(:pct 4 100 true))
   :stables (list '(:flat 60 false)
                  '(:flat 1 true))
   :dungeons (list '(:flat 20 false))
   :forts (list '(:pct 1.5 37.5 true))
   :gs (list '(:pct -2 -50 true))
   :barracks (list '(:pct -1.5 -37.5 true))
   :hospitals (list '(:pct 2 50 true)
                    '(:pct -3 -75 true))
   :guilds (list '(:flat 0.02 true))
   :libraries (list '(:pct 2 50 false))
   :tds (list '(:pct -4 -95 true)
              '(:pct 3 75 true))
   :wts (list '(:pct 2 50 true)
              '(:pct 3 75 true))
   :schools (list '(:pct -1.5 -37.5 true)
                  '(:pct -3.5 -87.5 true))
   :tgs (list '(:pct 1.5 37.5 true))
   :mills (list '(:pct -4 -99 true)
                '(:pct -3 -75 true))
   :banks (list '(:flat 25 true)
                '(:pct 1.25 31.25 true))
   :armouries (list '(:pct -2 -50 true)
                    '(:pct -2 -50 true)
                    '(:pct -1.5 -37.5 true))
   :towers (list '(:flat 12 true))
   :farms (list '(:flat 70 true))})

(defn pct-effect [n rate max-eff acres be]
  (let [pct (* rate (/ n acres) (- 1 (/ n acres)) be)]
    (cond (= 0 pct) 1.0
          (neg? pct) (+ 1 (max pct (/ max-eff 100)))
          :else (+ 1 (min pct (/ max-eff 100))))))

(defn flat-effect [n rate be]
  (* n rate be))

(defn jobs [prov]
  (let [buildings (dissoc (:buildings prov) :homes :barren :in-progress)]
    (* 25 (reduce + 0 (vals buildings)))))

(defn building-efficiency [prov]
  (let [workers (+ (:peasants prov) (floor (:prisoners prov)))
        opt-workers (/ (* 2 (jobs prov)) 3)
        jobs-filled (min (/ workers opt-workers) 1)]
    (+ (* 0.5 (+ 1 jobs-filled) 
          (sci-effect prov :build-eff)
          (if (= :fortified (:stance prov)) 1.2 1)
          (if (= :gold (:dragon prov)) 0.75 1))
       (if (= :dwarf (:race prov)) 0.25 0))))

(defn effect [prov building effect-num]
  (let [eff-list (nth (building effects-table) effect-num)]
    (if (= :pct (first eff-list))
      (if-not (building (:buildings prov)) 1.0
        (pct-effect (building (:buildings prov))
                    (second eff-list)
                    (nth eff-list 2)
                    (:acres prov)
                    (if (last eff-list) (:build-eff prov) 1.0)))
      (if-not (building (:buildings prov)) 0
        (flat-effect (building (:buildings prov))
                     (second eff-list)
                     (if (last eff-list) (:build-eff prov) 1.0))))))

(defn built-acres [prov]
  (+ (reduce + 0 (vals (dissoc (:buildings prov) :barren :in-progress)))
     (reduce + 0 (map #(reduce + 0 %) 
                      (vals (dissoc (:in-progress (:buildings prov)) 
                                    :barren))))))

(defmulti building-effect first)

(defmethod building-effect :homes [[_ n] acres be]
  (str "Houses " (* 33 n) " people and increases birthrate by "
       (format "%1$.2f" (pct-effect n 4 100 acres be)) "%."))

(defmethod building-effect :stables [[_ n] acres be]
  (str "Holds " (* 60 n) " horses and produces " 
       (format "%1$.2f" (flat-effect 1 be)) " horses daily"))

(defmethod building-effect :dungeons [[_ n] acres be]
  (str "Houses " (* 20 n) " prisoners."))

(defmethod building-effect :forts [[_ n] acres be]
  (str "Increases defensive military efficiency by "
       (format "%1$.2f" (pct-effect n 1.5 37.5 acres be)) "%."))

(defmethod building-effect :gs [[_ n] acres be]
  (str "Decreases resources lost when attacked by "
       (format "%1$.2f" (pct-effect n 2 50 acres be)) "%."))

(defmethod building-effect :barracks [[_ n] acres be]
  (str "Lowers attack time by " 
       (format "%1$.2f" (pct-effect n 1.5 37.5 acres be)) "%."))

(defmethod building-effect :hospitals [[_ n] acres be]
  (str (format "%1$.2f" (pct-effect n 2 50 acres be))
       "% daily chance of curing the plage.\n"
       "Decreases military losses by " 
       (format "%1$.2f" (pct-effect n 3 75 acres be)) "%."))

(defmethod building-effect :guilds [[_ n] acres be]
  (str (format "%1$.2f" (flat-effect n 0.02 be)) " wizards trained per day."))

(defmethod building-effect :libraries [[_ n] acres be] ;; Unaffected by BE
  (str "Increases the effects of science by "
       (format "%1$.2f" (pct-effect 2 50 1.0)
       "%.")))

(defmethod building-effect :tds [[_ n] acres be]
  (str "Reduces losses in thievery operations by "
       (format "%1$.2f" (pct-effect n 4 95 acres be)) "%.\n"
       "Increases thievery effectiveness by "
       (format "%1$.2f" (pct-effect n 3 75 acres be)) "%."))

(defmethod building-effect :wts [[_ n] acres be]
  (str (format "%1$.2f" (pct-effect n 2 50 acres be))
       "% chance of catching enemy thieves.\n"
       (format "%1$.2f" (pct-effect n 3 75 acres be))
       "% chance of repelling individual enemy thieves."))

(defmethod building-effect :schools [[_ n] acres be]
  (str "Decreases science costs by " 
       (format "%1$.2f" (pct-effect n 1.5 37.5 acres be)) "%.\n"
       "Protects " (format "%1$.2f" (pct-effect n 3.5 87.5 acres be))
       "% of books from learns."))

(defmethod building-effect :tgs [[_ n] acres be]
  (str "Increases offensive military efficiency by "
       (format "%1$.2f" (pct-effect n 1.5 37.5 acres be)) "%."))

(defmethod building-effect :mills [[_ n] acres be]
  (str "Decreases building costs by " 
       (format "%1$.2f" (pct-effect n 4 99 acres be)) "%.\n"
       "Decreases exploration costs by " 
       (format "%1$.2f" (pct-effect n 3 75 acres be)) "%."))

(defmethod building-effect :banks [[_ n] acres be]
  (str "Produces " (format "%1$.2f" (flat-effect n 25 be)) "gc per day.\n"
       "Increases income by " 
       (format "%1$.2f" (pct-effect n 1.25 31.25 acres be)) "%."))

(defmethod building-effect :armouries [[_ n] acres be]
  (str "Reduces military wages and draft costs by "
       (format "%1$.2f" (pct-effect n 2 50 acres be)) "%.\n"
       "Reduces training costs by " 
       (format "%1$.2f" (pct-effect n 1.5 37.5 acres be)) "%."))

(defmethod building-effect :towers [[_ n] acres be]
  (str "Produces " (format "%1$.2f" (flat-effect n 12 be)) " runes per day."))

(defmethod building-effect :farms [[_ n] acres be]
  (str "Produces " (format "%1$.2f" (flat-effect n 70 be))
       " bushels per day."))

(defmethod building-effect :barren [[_ n] acres be]
  (str "Produces " (* 2 n) " bushels per day.\n"
       "Houses " (* 10 n) " people."))

(defn building-effects [prov]
  (join "\n" (map building-effect 
                  (:buildings prov)
                  (repeat (building-efficiency prov))
                  (repeat (:acres prov)))))
