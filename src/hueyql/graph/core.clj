(ns hueyql.graph.core
  (:require [clojure.set :as set]
            [datomic.api :as d]))


(defmulti execute (fn [root fields context] (first root)))

(defn hydrate [graph context]
  (into {}
        (map (fn [[root attributes]] [root (execute root attributes context)])
             graph)))

(defn keywordize-root [root]
  (update-in root [0] keyword))

(defn keywordize-fields [fields]
  (into #{} (map (fn [field]
                   (if (vector? field)
                     (let [[field fields] field]
                       [(keyword field) (keywordize-fields fields)])
                     (keyword field)))
                 fields)))

(defn parse-roots [roots]
  (into {} (map (fn [[root fields]]
                  [(keywordize-root root) (keywordize-fields fields)])
                roots)))
