(ns huql.graph.core
  (:require [schema.core :as s]
            [schema.coerce :as coerce]
            [manifold.deferred :as d]
            [manifold.stream :as stream]
            [huql.github.core :as gh]
            [huql.github.client :as client]
            [huql.graph.query :as query]
            [clj-time.format :as f]
            [clj-time.core :as t]))

(defrecord FieldDefinition [name type executor cardinality args calls])

(defrecord Node [type fields calls])

(defrecord RootDefinition [name type executor cardinality id])

(defn node-descriptor-exec [node-name graph]
  (get-in graph [:nodes node-name]))

(defmacro defnode [type & fields]
  (list 'def type (map->Node (merge {:type type}
                                    (reduce (fn [fc [x & rest]]
                                              (condp = x
                                                'field (let [[name type executor & opts] rest
                                                             options (when opts (apply assoc (conj opts {})))]
                                                         (assoc-in fc [:fields name] (map->FieldDefinition (eval (merge {:name name :type type :executor executor :cardinality :one} options)))))
                                                'call (let [[name & opts] rest
                                                            options (when opts (apply assoc (conj opts {})))]
                                                        (assoc-in fc [:calls name] (eval options)))))
                                            {:fields {:__type__ (map->FieldDefinition {:name :__type__ :type 'NodeDescriptor :executor (partial node-descriptor-exec type) :cardinality :one})}}
                                            fields)))))

(defn root [name type executor & options]
  (map->RootDefinition (merge {:cardinality :one}
                              {:name name :type type :executor executor}
                              (when options (apply assoc (conj options {}))))))

(defn add-node [graph node]
  (assoc-in graph [:nodes (:type node)] node))

(defn add-root [graph root]
  (assoc-in graph [:roots (:name root)] root))

(defnode FieldDescriptor
  (field :name 'string :name)
  (field :description 'string :description)
  (field :type 'string :type))

(defnode NodeDescriptor
  (field :name 'string :type)
  (field :description 'string :description)
  (field :fields 'FieldDescriptor (comp vals :fields)
         :cardinality :many))

(def graph (-> {:roots {} :nodes {}}
             (add-node NodeDescriptor)
             (add-node FieldDescriptor)))


(defn execute [graph field query value]
  (let [args (concat [value] (:args query) (:calls query))
        result (if (= :__type__ (:name field))
                 ((:executor field) graph)
                 (apply (:executor field) args))]
    (condp = (:cardinality field)
      :one (d/chain result)
      :many (stream/->source result))))

(defn executor [w value path graph field query]
  (let [value (execute field query value)]
    (w value)))

(defn async-walker' [f graph field query path value]
  (if (and (seq (:fields query)) (not (nil? value)))
    (d/let-flow [kvs (apply d/zip (map (fn [query]
                                         (d/let-flow [field (get-in graph [:nodes (:type field) :fields (:name query)])
                                                      value (f value (conj path (:name field)) graph field query)]
                                                     [(or (:alias query) (:name query)) value]))
                                       (:fields query)))]
                (into {} kvs))
    value))

(defn async-walker [f graph field query path]
  (fn [value]
    (condp = (:cardinality field)
      :one (async-walker' f graph field query path value)
      :many (->> (stream/->source value)
              (stream/transform (comp (map-indexed (fn [idx value]
                                                     (async-walker' f graph field query (conj path idx) value)))))
              (stream/buffer 50)
              (stream/realize-each) ;; reduce seems to do this but wutevs
              (stream/reduce conj [])))))

(defn walk
  ([executor graph query]
   (d/let-flow [root (get-in graph [:roots (:name query)])
                kvs (apply d/zip (map (fn [id]
                                        (d/let-flow [value (walk executor id [id] graph root query)]
                                                    [id value]))
                                      (:ids query)))]
               (into {} kvs)))
  ([executor value path graph field query]
   (executor (async-walker (partial walk executor)
                           graph
                           field
                           query
                           path)
             value
             path
             graph
             field
             query)))

(defn run [graph query-string]
  (let [query (query/parse graph query-string)]
    (walk executor graph query)))


(defn profiled-executor [pd]
  (let [base (System/nanoTime)]
    (fn [w value path graph field query]
      (swap! pd assoc-in [path :start] (/ (- (System/nanoTime) base) 1000000.0))
      (let [value (execute graph field query value)
            result (w value)]
        (condp = (:cardinality field)
          :one (d/chain value (fn [value]
                                (swap! pd assoc-in [path :execute] (/ (- (System/nanoTime) base) 1000000.0))))
          :many (stream/on-drained value (fn []
                                           (swap! pd assoc-in [path :execute] (/ (- (System/nanoTime) base) 1000000.0)))))
        (d/chain result (fn [value]
                          (swap! pd update-in [path :execute] #(or % (/ (- (System/nanoTime) base) 1000000.0)))
                          (swap! pd assoc-in [path :end] (/ (- (System/nanoTime) base) 1000000.0))))
        result))))

(defn run-profiled [pkey graph query-string]
  (let [query (query/parse graph query-string)
        profile-data (atom {})
        executor (profiled-executor profile-data)]
    (d/chain (walk executor graph query)
             #(assoc % pkey @profile-data))))
