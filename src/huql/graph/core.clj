(ns huql.graph.core
  (:require [instaparse.core :as insta]
            [clojure.set :as set]
            [schema.core :as s]
            [schema.coerce :as coerce]
            [manifold.deferred :as d]))

(def graph {:roots {} :nodes {} :edges {}})

(defn tuple
  [& schema-name-pairs]
  (->> schema-name-pairs
    (mapv (fn [pair]
            (if (map? pair)
              (apply s/optional (-> pair seq first))
              (apply s/one pair))))))

(defn ->edge [opts]
  (if-let [args-schema (:args opts)]
    (assoc opts :parse-args (coerce/coercer args-schema coerce/string-coercion-matcher))
    opts))

(defmacro defnode [type & fields]
  (list 'def type {:type `(quote ~type)
                   :fields (into {} (map (fn [[x name type executor & opts]]
                                           (assert (= 'field x))
                                           (let [options (when opts (apply assoc (conj opts {})))]
                                             [name (list `->edge (merge {:type type :executor executor :cardinality :one} options))]))
                                         fields))}))

(defn add-node [graph node]
  (let [type (:type node)
        graph (assoc-in graph [:nodes type] (select-keys node [:type]))
        graph (reduce (fn [graph [name edge]]
                        (assoc-in graph [:edges type name] edge))
                      graph
                      (:fields node))]
    graph))

(defn add-root [graph root]
  (let [key (:key root)]
    (assoc-in graph [:roots key] (dissoc root :key))))

(defn field-key [field]
  (or (:alias field) (:name field)))

(defn scalar? [graph edge]
  (empty? (get-in graph [:edges (:type edge)])))

(defn execute [path edge query input]
  (let [args [input]
        args (if-let [query-args (:args query)]
               (concat args ((:parse-args edge) query-args))
               args)]
    (apply (:executor edge) args)))

(defn query-key [query]
  (or (:alias query) (:name query)))

(defn query-edge [graph type query]
  (get-in graph [:edges type (:name query)]))

(defn field-edge [graph type field]
  (get-in graph [:edges type (:name field)]))

(defn field-key [field]
  (or (:alias field) (:name field)))

(declare expand)

(defn expand-fields [graph executer path type fields input]
  (let [kvs (map (fn [field]
                   (let [edge (field-edge graph type field)]
                     (d/chain (expand graph executer (conj path (field-key field)) edge field input)
                              (fn [value]
                                [(field-key field) value]))))
                 fields)]
    (d/chain (apply d/zip kvs) (fn [kvs]
                                 (into {} kvs)))))

(defn expand [graph executer path edge query input]
  (let [type (:type edge)
        value (d/chain input (partial executer path edge query))]
    (d/chain value
             (fn [value]
               (if (or (nil? value) (scalar? graph edge))
                 value
                 (condp = (:cardinality edge)
                   :one (let [expanded (expand-fields graph executer path type (:fields query) value)]
                          expanded)
                   :many (apply d/zip (map-indexed (fn [idx value]
                                                     (expand-fields graph executer (conj path idx) type (:fields query) value))
                                                   value))))))))


(def query-parser
  (insta/parser "ROOT = <whitespace> NAME ARGS? FIELDS <whitespace>
                 NAME = token
                 ARGS = <whitespace> <'('> ARG (<','> ARG)* <')'> <whitespace>
                 <ARG> = <whitespace> #'[^,)]+' <whitespace>
                 FIELDS = <whitespace> <'{'> FIELD (<','> FIELD)* <'}'> <whitespace>
                 FIELD = <whitespace> NAME(ARGS | CALLS)? <whitespace> (<'as'> <whitespace> ALIAS <whitespace>)? FIELDS? <whitespace>
                 ALIAS = token
                 CALLS = CALL+
                 CALL = <'.'> NAME ARGS
                 <token> = #'\\w+'
                 whitespace = #'\\s*'"))

(def query-transform
  (partial insta/transform {:NAME (fn [name] [:name (keyword name)])
                            :ARGS (fn [& args] [:args (vec args)])
                            :ALIAS (fn [alias] [:alias (keyword alias)])
                            :CALLS (fn [& calls] [:filters (vec calls)])
                            :CALL (fn [name args] (into {} [name args]))
                            :FIELDS (fn [& fields] [:fields (vec fields)])
                            :FIELD (fn [& args] (into {} args))
                            :ROOT (fn [& args] (into {} args))}))

(defn parse-query [string]
  (-> string
    query-parser
    query-transform))

(defn root->edge [root]
  (->edge (merge {:cardinality :one} (select-keys root [:type :executor :args]))))


(defn validate-scalar [graph edge query]
  (when (scalar? graph edge)
    (let [query-fields (map :name (:fields query))]
      (when (seq query-fields)
        (throw (Exception. (str "scalar type '" (:type edge) "' cannot have fields : " query-fields))))))
  query)

(defn validate-has-fields [graph edge query]
  (when (not (scalar? graph edge))
    (let [query-fields (map :name (:fields query))]
      (when (empty? query-fields)
        (throw (Exception. (str "non-scalar type '" (:type edge) "' must have fields."))))))
  query)

(defn validate-known-fields [graph edge query]
  (let [type (:type edge)
        type-fields (into #{} (keys (get-in graph [:edges type])))
        query-fields (into #{} (map :name (:fields query)))
        unknown-fields (set/difference query-fields type-fields)]
    (when (seq unknown-fields)
      (throw (Exception. (str "unknown fields for type '" type "': " unknown-fields)))))
  query)

(defn validate-args [graph edge query]
  (let [type (:type edge)
        query-args (:args query)
        edge-args (:args edge)]
    (cond
      (and (nil? query-args) (nil? edge-args)) query
      (nil? query-args) (throw (Exception. (str "args expected for '" type "': " (s/explain edge-args))))
      (nil? edge-args) (throw (Exception. (str "args are not valid for '" type "' : " query-args)))
      :else (update-in query [:args] (:parse-args edge)))))

(defn validate-query [graph edge query]
  (let [type (:type edge)
        scalar (scalar? graph edge)
        query (reduce (fn [query vf]
                        (vf graph edge query))
                      query
                      [validate-scalar validate-has-fields validate-known-fields validate-args])]
    (cond
      scalar query
      :else (update-in query [:fields] #(mapv (fn [field]
                                                (let [edge (field-edge graph type field)]
                                                  (validate-query graph edge field)))
                                              %)))))

(defn validate [graph query]
  (if (insta/failure? query)
    (throw (Exception. (pr-str (insta/get-failure query))))
    (let [root (get-in graph [:roots (:name query)])]
      (if root
        (do
          (prn [:root root :query query])
          (validate-query graph (root->edge root) query))
        (throw (Exception. (str "unknown root: " (name (:name query)))))))))

(defn profiled-executor [data next]
  (fn [path edge query input]
    (let [start (/ (System/nanoTime) 1000000.0)
          result (next path edge query input)
          end (/ (System/nanoTime) 1000000.0)
          duration (- end start)]
      (d/chain result (fn [x]
                        (let [end (/ (System/nanoTime) 1000000.0)
                              duration (- end start)]
                          (swap! data assoc path {:start start :end end :duration duration}))))
      result)))

(defn run [graph query-string]
  (let [query (parse-query query-string)
        query (validate graph query)
        root (get-in graph [:roots (:name query)])
        roots ((:roots root) query)
        result (into {} (map (fn [[key input]]
                               [key @(expand graph execute [key] (root->edge root) (dissoc query :args) input)])
                             roots))]
    result))

(defn run-profiled [graph query-string]
  (let [query (parse-query query-string)
        query (validate graph query)
        root (get-in graph [:roots (:name query)])
        roots ((:roots root) query)
        profile (atom {})
        result @(d/chain (apply d/zip (map (fn [[key input]]
                                             (d/chain (expand graph (profiled-executor profile execute) [key] (root->edge root) (dissoc query :args) input)
                                                      (fn [value]
                                                        [key value])))
                                           roots))
                         (partial into {}))
        profile @profile
        start (apply min (map (fn [[key data]] (:start data)) profile))
        profile (map (fn [[path data]]
                       {:path (pr-str path)
                        :start (- (:start data) start)
                        :end (- (:end data) start)})
                     profile)]
    (assoc result :profile-data profile)))
