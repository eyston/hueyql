(ns hueyql.graph.datomic
  (:require [hueyql.graph.core :as graph]
            [datomic.api :as d]
            [clojure.core.match :refer [match]]))

(defmulti mapper (fn [field entity context] field))

(defmethod mapper :default [field entity context]
  (get entity field))

(def expand-entity)

(defn ->one-to-many [seq]
  {:count (count seq)
   :edges (map (fn [item] {:cursor (:db/id item)
                           :node item})
               seq)})

(defn expand-collection [collection fields context]
  (mapv (fn [entity] (expand-entity entity fields context))
        (take 15 collection)))

(defn expand-entity [entity fields context]
  (into {}
        (map (fn [field]
               (match [field]
                      [[key ref-fields]] (let [ref (mapper key entity context)]
                                           (cond
                                             (set? ref) [key (expand-entity (->one-to-many ref) ref-fields context)]
                                             (seq? ref) [key (expand-collection ref ref-fields context)]
                                             :else [key (expand-entity ref ref-fields context)]))
                      [key] [key (mapper key entity context)]))
             fields)))


(defmethod graph/execute :entity [[_ id] fields {:keys [db] :as context}]
  (expand-entity (d/entity db id) fields context))

(defmethod graph/execute :node [[_ id] fields {:keys [db] :as context}]
  (expand-entity (d/entity db id) fields context))

(defmethod graph/execute :viewer [_ fields {:keys [db user] :as context}]
  (expand-entity user fields context))
