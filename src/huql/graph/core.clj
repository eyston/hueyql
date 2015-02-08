(ns huql.graph.core
  (:require [schema.core :as s]
            [schema.utils :as su]
            [schema.macros :as sm]))

(defonce ROOTS (atom {}))

(defprotocol IExecutor
  (execute [this schema]))

(defn executor [f & args]
  (reify IExecutor
    (execute [_ _] (apply f args))))

(def primitive? #{s/Str s/Bool s/Int})

(defrecord CollectionObject [schema]
  s/Schema
  (walker [this]
    (let [walker (s/subschema-walker schema)]
      (fn [x]
        (let [x (if (:edges x) (update-in x [:edges] vector) x)
              result (walker x)]
          result))))
  (explain [this]
    (list 'collection-object (s/explain schema))))

(defn collection-object [filters cursor node]
  (CollectionObject. {:filters filters
                      :count s/Int
                      :edges [{:cursor cursor
                               :node node}]}))

(defn graph-walker [schema]
  (s/start-walker
   (fn [s]
     (let [walk (s/walker s)]
       (fn [x]
         (if (and (primitive? s) (nil? x))
           s
           (let [result (walk x)]
             result)))))
   schema))

(defn optionalize [schema]
  (cond
    (#{s/Int s/Bool s/Str} schema) schema
    (instance? CollectionObject schema) (let [schema (:schema schema)
                                              filters (:filters schema)
                                              filters (into {} (map (fn [[filter-key filter-params]]
                                                                      [(s/optional-key filter-key) filter-params])
                                                                    filters))
                                              schema (-> schema
                                                       (dissoc :filters)
                                                       optionalize
                                                       :schemas
                                                       first
                                                       (assoc (s/optional-key :filters) filters)
                                                       (s/both (s/pred seq 'not-empty)))]
                                          (CollectionObject. schema))
    (map? schema) (s/both (into {} (map (fn [[k v]]
                                          [(s/optional-key k) (optionalize v)])
                                        schema))
                          (s/pred seq 'not-empty))
    (vector? schema) [(optionalize (first schema))]
    :else schema))

(defn build [schema call]
  (let [graph ((graph-walker (optionalize schema)) call)]
    (if (su/error? graph)
      (sm/error! (su/format* "Value does not match schema: %s" (pr-str graph))
                 {:schema schema :value call :error graph}))
    graph))

(defn walk [schema data]
  (let [data (if (satisfies? IExecutor data) (execute data schema) data)]
    (cond
      (primitive? schema) data
      (map? schema) (into {} (map (fn [[schema-key schema-val]]
                                    [schema-key (walk schema-val (get data schema-key))])
                                  schema))
      (vector? schema) (let [schema (first schema)]
                         (mapv (fn [item]
                                 (walk schema item))
                               data))
      :else data)))

(defn register [key handler]
  (swap! ROOTS assoc key handler))

(defn execute-roots [roots]
  (mapv (fn [[root call]]
          (let [key (first root)
                handler (get @ROOTS key)]
            [root (handler root call)]))
        roots))
