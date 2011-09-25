(ns utopia-sim.core
  (:use compojure.core
        [utopia-sim create welcome]
        [clojure.pprint :only [pprint]])
  (:require [compojure.route :as route]
            [compojure.handler :as handler]))

(def *prov* (atom {}))

(defroutes main-routes
  (GET "/" [] (welcome-page))
  (POST "/create" [race personality prov-name ruler-name gender monarch?]
    (do (reset! *prov* (new-prov (keyword race) (keyword personality) 
                                prov-name ruler-name (keyword gender)
                                 (if (= "yes" monarch?) true false)))
      (println monarch?)
      (create @*prov*)))
  ;(GET "/throne" [] (throne-page prov))
  (route/files "/")
  (route/not-found "Page not found"))

(def app
  (handler/site main-routes))
