(ns utopia-sim.welcome
  (:use [hiccup core page-helpers form-helpers]
        [utopia-sim create util honor]
        [clojure.pprint :only [pprint]]))

(defn welcome-page []
  (view-layout
    [:p "Please choose your province attributes."]
    [:p
     (form-to [:POST "/create"]
       [:p "Race: " 
        (drop-down :race
                   [:human :halfling :elf :dwarf :orc :avian :undead :faery])]
       [:p "Personality: " 
        (drop-down :personality
                   [:merchant :shepherd :sage :rogue :mystic :warrior
                    :tactician :cleric])]
       [:p "Province name: " (text-field :prov-name)]
       [:p "Ruler name: " (text-field :ruler-name)]
       [:p "Gender: " (drop-down :gender [:male :female])]
       [:p "Monarch? " (drop-down :monarch? [:yes :no])]
       [:p (submit-button "Submit")])]))

(defn create [prov]
  (view-layout
    (str "You created a province called " (:prov-name prov)
         ", a " (strip-colon (:race prov)) 
         " " (strip-colon (:personality prov))
         ". Welcome, " ((if (= :male (:gender prov)) second #(nth % 2)) 
                          (rank prov))
         " " (:ruler-name prov) "!")))
