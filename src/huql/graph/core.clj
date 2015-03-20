(ns huql.graph.core
  (:require [schema.core :as s]
            [schema.coerce :as coerce]
            [manifold.deferred :as d]
            [manifold.stream :as stream]
            [huql.github.core :as gh]
            [huql.github.client :as client]
            [huql.graph.query :as query]
            [clj-time.format :as f]
            [clj-time.core :as t]
            [clojure.edn :as edn])
  (:refer-clojure :exclude [key compile type]))

(defn scalar [name parse]
  [{:graph/type :type
    :type/type :scalar
    :name name
    :parse parse}])

(defn type [name & args]
  (let [fields (filter (comp (partial = :field) first) args)
        calls (filter (comp (partial = :call) first) args)
        calls (into {} (map (fn [[_ name args]] [name {:name name
                                                       :args args}]) calls))]
    (vec (concat [(merge {:graph/type :type
                          :type/type :complex
                          :name name}
                         ;; TODO: make calls a top level graph construct like fields perhaps, e.g. (get-in graph [:calls 'Repositories])
                         (when (seq calls) {:calls calls}))
                  {:graph/type :field
                   :field/type :descriptor
                   :name :__type__
                   :type 'TypeDescriptor
                   :source name}]
                 (mapv (fn [[_ fname type executor & opts]]
                         (let [opts (when opts (apply assoc (conj opts {})))]
                           (merge {:graph/type :field
                                   :field/type :property
                                   :name fname
                                   :type type
                                   :source name
                                   :executor executor
                                   :cardinality :one}
                                  opts)))
                       fields)))))

(defn root [name type executor root-type & [id]]
  [(merge {:graph/type :root
           :root/type root-type
           :name name
           :type type
           :executor executor
           :cardinality :one}
          (when id {:id id}))])

(def FieldDescriptor
  (type 'FieldDescriptor
        [:field :name 'string :name]
        [:field :description 'string :description]
        [:field :type 'string :type]))

(def TypeDescriptor
  (type 'TypeDescriptor
        [:field :name 'string :name]
        [:field :description 'string :description]
        [:field :fields 'FieldDescriptor :fields
         :cardinality :many]))

(defn insert [graph items]
  (reduce (fn [graph item]
            (condp = (:graph/type item)
              :root (assoc-in graph [:roots (:name item)] item)
              :type (assoc-in graph [:types (:name item)] item)
              :field (assoc-in graph [:fields (:source item) (:name item)] item)))
          graph
          items))

(def base-graph (-> {}
                  ;; add scalars as needed
                  (insert (scalar 'string identity))
                  (insert (scalar 'integer (fn [s]
                                             (let [i (edn/read-string s)]
                                               (if (integer? i)
                                                 i
                                                 (throw (Exception. "not an integer")))))))
                  (insert (scalar 'boolean identity)) ;; todo: make parser yo
                  
                  ;; for schema descriptions
                  (insert FieldDescriptor)
                  (insert TypeDescriptor)))

(defn ->graph [& items]
  ;; todo: add validation after inserts that this graph is legit
  ;; - scalars have no fields
  ;; - complex do
  ;; - field / root types exist
  ;; - etc ...
  (insert base-graph (mapcat identity items)))

(defprotocol Executable
  (execute [this value])
  (cardinality [this])
  ;; not sure about these!
  (key [this]) ;; root kind of doesn't want this ...
  (fields [this])) ;; they all need this, but not really to do with being executable

(defrecord ExecutableRoot [root id fields]
  Executable
  (execute [_ _]
    ((:executor root) id))
  (cardinality [_] (:cardinality root))
  (key [_] id)
  (fields [_] fields))

(defrecord ExecutableField [field query fields]
  Executable
  (execute [_ value]
    (let [args (concat [value] (:args query) (:calls query))]
      (apply (:executor field) args)))
  (cardinality [_] (:cardinality field))
  (key [_] (or (:alias query) (:name query)))
  (fields [_] fields))

(defrecord ExecutableTypeDescriptor [graph field query fields]
  Executable
  (execute [_ _]
    (let [type (get-in graph [:types (:source field)])
          fields (vals (get-in graph [:fields (:source field)]))]
      (assoc type :fields fields)))
  (cardinality [_] :one)
  (key [_] (or (:alias query) (:name query)))
  (fields [_] fields))


(defn compile
  ([graph query]
   (let [root (get-in graph [:roots (:name query)])]
     (compile graph query root)))
  ([graph query field]
   (let [fields (mapv (fn [query]
                        (let [field (get-in graph [:fields (:type field) (:name query)])]
                          (compile graph query field)))
                      (:fields query))]
     (condp = (:graph/type field)
       ;; replace with multimethod mebe -- a compile per graph/type sub/type
       ;; e.g. [:root :one], [:root :ident], [:field :property], etc
       ;; would allow extending types
       ;; validate could also be this same pattern
       :root (let [root-type (:root/type field)]
               (cond
                 (= :ident root-type) [(->ExecutableRoot field (:name field) fields)]
                 (or (= :one root-type)
                     (= :many root-type)) (mapv (fn [id]
                                                  (->ExecutableRoot field id fields))
                                                (:ids query))))
       :field (condp = (:field/type field)
                :property (->ExecutableField field query fields)
                :descriptor (->ExecutableTypeDescriptor graph field query fields))))))

(defn async-execute [field value]
  (let [result (execute field value)]
    ;; coerce to manifold friendly yo
    (condp = (cardinality field)
      :one (d/chain result)
      :many (stream/->source result))))

(defn async-walker' [f field path value]
  (d/let-flow [value value]
              (if (and (seq (fields field)) (not (nil? value)))
                (d/let-flow [kvs (apply d/zip (map (fn [field]
                                                     (d/let-flow [value (f value (conj path (key field)) field)]
                                                                 [(key field) value]))
                                                   (fields field)))]
                            (into {} kvs))
                value)))

(defn async-walker [f path field]
  (fn [value]
    (condp = (cardinality field)
      :one (async-walker' f field path value)
      :many (->> (stream/->source value)
              (stream/transform (comp (map-indexed (fn [idx value]
                                                     (async-walker' f field (conj path idx) value)))))
              (stream/buffer 50) ;; we need to buffer results so it does not wait for deferreds to resolve to do next
              (stream/realize-each) ;; reduce seems to do this but wutevs
              (stream/reduce conj [])))))

;; walk maybe part of protocol? only really different for roots vs fields and then just barely
(defn walk
  ([executor roots]
   (d/let-flow [kvs (apply d/zip
                           (map (fn [root]
                                  (d/let-flow [value (walk executor nil [(key root)] root)]
                                              [(key root) value]))
                                roots))]
               (into {} kvs)))
  ([executor value path field]
   (let [walker (async-walker (partial walk executor) path field)]
     (executor walker value path field))))

(defn executor [w value path field]
  (let [value (async-execute field value)]
    (w value)))

;; throws on errors
(defn string->compiled [graph query-string]
  (->> query-string
    query/parse
    (query/validate graph)
    (compile graph)))

(defn run [graph query-string]
  (let [query (string->compiled graph query-string)]
    (walk executor query)))

(defn profiled-executor [pd]
  (let [base (System/nanoTime)]
    (fn [w value path field]
      (swap! pd assoc-in [path :start] (/ (- (System/nanoTime) base) 1000000.0))
      (let [value (async-execute field value)
            result (w value)]
        (condp = (cardinality field)
          :one (d/chain value (fn [value]
                                (swap! pd assoc-in [path :execute] (/ (- (System/nanoTime) base) 1000000.0))))
          :many (stream/on-drained value (fn []
                                           (swap! pd assoc-in [path :execute] (/ (- (System/nanoTime) base) 1000000.0)))))
        (d/chain result (fn [value]
                          ;; sometimes end resolves before execute so we set if nil
                          (swap! pd update-in [path :execute] #(or % (/ (- (System/nanoTime) base) 1000000.0)))
                          (swap! pd assoc-in [path :end] (/ (- (System/nanoTime) base) 1000000.0))))
        result))))

(defn run-profiled [pkey graph query-string]
  (let [query (string->compiled graph query-string)
        profile-data (atom {})
        executor (profiled-executor profile-data)]
    (d/chain (walk executor query)
             #(assoc % pkey @profile-data))))
