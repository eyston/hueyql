(ns lending-relay.graph
  (:require [clojure.set :as set]
            [datomic.api :as d]
            [clojure.walk :refer [walk keywordize-keys]]))

(defmulti query (fn [context root] (first root)))
(defmulti mapper (fn [field user context] field))
(defmulti viewer (fn [name options fields context] name))

(defn parse
  ([context]
   (parse :roots context))
  ([context graph]
   (condp = context
     :roots (mapv (partial parse :root) graph)
     :root (let [type (first graph)]
             (condp = type
               "viewer" (let [[_ name options fields] graph]
                          [:viewer (keyword name) (keywordize-keys options) (parse :fields fields)])
               "node" (let [[_ id fields] graph]
                        [:node id (parse :fields fields)])))
     :fields (walk #(cond (string? %) (keyword %)
                          (vector? %) [(keyword (first %)) (parse :fields (second %))])
                   #(into #{} %)
                   graph))))

(defmethod mapper :count [_ entities _]
  (count entities))

(defmethod mapper :nodes [_ entities _]
  (into #{} (map :db/id entities)))

(def expand-entity)

(defn ref->one [context parent-id parent-field entity fields]
  (into [[parent-id parent-field (:db/id entity)]]
        (map (fn [field]
               [(:db/id entity) field (mapper field entity context)])
             fields)))

(def META-KEYS #{:count :nodes})

(def gen-id ((fn []
               (let [id (atom -1)]
                 (fn []
                   (swap! id dec))))))


(defn ref->many [context parent-id parent-field entities fields]
  (let [id (gen-id)
        entity-fields (set/difference fields META-KEYS)
        meta-fields (set/intersection META-KEYS (set/union fields (when (seq entity-fields) #{:nodes})))]
    (into [] (concat [[parent-id parent-field id]]
                     (map (fn [field] [id field (mapper field entities context)]) meta-fields)
                     (mapcat (fn [entity]
                               [[entity entity-fields]]
                               (expand-entity context entity entity-fields))
                             entities)))))

(defn expand-entity [context entity fields]
  (vec (mapcat (fn [field]
                 (if (vector? field)
                   (let [[field nested-fields] field
                         nested-entity (mapper field entity context)]
                     (if (set? nested-entity)
                       (ref->many context (:db/id entity) field nested-entity nested-fields)
                       (ref->one context (:db/id entity) field nested-entity nested-fields)))
                   [[(:db/id entity) field (mapper field entity context)]]))
               fields)))

(defmethod query :node [{:keys [:db] :as context} [_ id fields]]
  (let [entity (d/entity db id)]
    (expand-entity context entity fields)))

(defmethod query :viewer [context [_ name options fields]]
  (viewer name options fields context))

(defn datoms [context roots]
  (into #{} (mapcat (partial query context) roots)))

(defn entities [datoms]
  (into {} (map (fn [[id datoms]]
                  [id (into {} (map (fn [[_ a v]] [a v]) datoms))])
                (group-by first datoms))))

(defn hydrate-entity [id fields entities]
  (let [entity (get entities id)]
    (into {} (map (fn [field]
                    (cond (keyword? field) [field (get entity field)]
                          (vector? field) (let [[field child-fields] field
                                                cid (get entity field)]
                                            [field (hydrate-entity cid child-fields entities)])))
                  fields))))

(defn hydrate-viewer [viewer entities]
  (let [[_ name options fields] viewer
        id (get-in entities [name :viewer])]
    [:viewer name options (mapv (fn [node-id]
                                  (hydrate-entity node-id fields entities))
                                (get-in entities [id :nodes]))]))

(defn hydrate-node [node entities]
  (let [[_ id fields] node]
    (hydrate-entity id fields entities)))

(defn hydrate [context roots]
  (let [datoms (datoms context roots)
        entities (entities datoms)]
    (mapv (fn [root] (condp = (first root)
                       :viewer (hydrate-viewer root entities)
                       :node (hydrate-node root entities)))
          roots)))

;; (let [graph [[:viewer :friends {:count 10} #{:user/name :user/is-verified :user/profile-pic [:user/mutual-friends #{:count}] :db/id}]]
;;       db (d/db (d/connect "datomic:mem://users"))
;;       context {:db db
;;                :current-user (d/entity db 17592186045434)}]
;;   (hydrate context graph))
