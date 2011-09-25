(defproject utopia-sim "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [compojure "0.6.4"]
                 [hiccup "0.3.6"]]
  :dev-dependencies [[lein-ring "0.4.5"]]
  :ring {:handler utopia-sim.core/app})
