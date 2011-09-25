(ns utopia-sim.util
  (:use [clojure.contrib.math :only [floor]]
        [hiccup core page-helpers]))

(defn view-layout [& content]
  (html
    (doctype :xhtml-strict)
    (xhtml-tag "en"
      [:head
       [:meta {:http-equiv "Content-type"
               :content "text/html; charset=utf-8"}]
       (include-css "style/reset.css"
                    "style/typography.css"
                    "style/wireframe_internal.css"
                    "style/original.css"
                    "style/tabs.css")
       [:title "Province Simulator"]]
      [:body content])))

(defn strip-colon [k]
  (apply str (rest (str k))))

(def empty-progress (vec (repeat 24 0)))

(defn if-mult [p x]
  (if p x 1))

(defn spread [v k]
  (let [n (count v)
        m (quot k n)
        r (rem k n)]
    (loop [coll (vec (map #(+ % m) v))
           i 0]
      (if (or (= i n) (zero? r)) coll
        (let [idx (int (floor (+ i (rand-int (/ n r)))))]
          (recur (assoc coll idx (inc (get coll idx)))
                 (+ i (/ n r))))))))
