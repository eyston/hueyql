(ns huql.graph.query
  (:require [instaparse.core :as insta]
            [clojure.set :as set]
            [clojure.edn :as edn]))

(def query-parser
  (insta/parser "ROOT = <whitespace> NAME IDS? FIELDS <whitespace>
                 NAME = token
                 IDS = <whitespace> <'('> ARG (<','> ARG)* <')'> <whitespace>
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
                            :IDS (fn [& args] [:ids (vec args)])
                            :ARGS (fn [& args] [:args (vec args)])
                            :ALIAS (fn [alias] [:alias (keyword alias)])
                            :CALLS (fn [& calls] [:calls (vec calls)])
                            :CALL (fn [name args] (into {} [name args]))
                            :FIELDS (fn [& fields] [:fields (vec fields)])
                            :FIELD (fn [& args] (into {} args))
                            :ROOT (fn [& args] (into {} args))}))

(defn walk-query [f query graph field path]
  (let [path (conj path (name (:name query)))]
    (if (seq (:fields query))
      (update-in (f query graph field path) [:fields] (partial mapv (fn [query]
                                                                      (let [field (get-in graph [:nodes (:type field) :fields (:name query)])]
                                                                        (walk-query f query graph field path)))))
      (f query graph field path))))

(defn complex? [graph field]
  (contains? (:nodes graph) (:type field)))

(defn scalar? [graph field]
  (not (complex? graph field)))

(defn validate-scalar [query graph field path]
  (if (and (scalar? graph field) (seq (:fields query)))
    (throw (Exception. (str "Scalar types cannot have fields. " (clojure.string/join "." path) " type " (:type field) " has invalid fields: " (clojure.string/join ", " (map (comp name :name) (:fields query))) ".")))
    query))

(defn validate-has-fields [query graph field path]
  (if (and (complex? graph field) (empty? (:fields query)))
    (throw (Exception. (str "Complex types must have fields. " (clojure.string/join "." path) " type " (:type field) " has no fields.  Valid fields are: " (clojure.string/join ", " (map name (keys (get-in graph [:nodes (:type field) :fields])))))))
    query))

(defn validate-known-fields [query graph field path]
  (if (complex? graph field)
    (let [node (get-in graph [:nodes (:type field)])
          node-fields (into #{} (keys (:fields node)))
          query-fields (into #{} (map :name (:fields query)))
          unknown-fields (set/difference query-fields node-fields)]
      (if (seq unknown-fields)
        (throw (Exception. (str "Unknown fields. " (clojure.string/join "." path) " type " (:type field) " has unknown fields: " (clojure.string/join ", " (map name unknown-fields)))))
        query))
    query))

(defn validate-complex [query graph field path]
  (-> query
    (validate-has-fields graph field path)
    (validate-known-fields graph field path)))

(defn coerce-argument [field-arg query-arg]
  (condp = field-arg
    'string query-arg
    'integer (let [i (edn/read-string query-arg)]
               (if (integer? i)
                 i
                 (throw (Exception. "not an integer"))))))

(defn validate-arguments [query graph field path]
  (let [field-args (:args field)
        query-args (:args query)]
    (if (not= (count field-args) (count query-args))
      (cond
        (= (count field-args) 0) (throw (Exception. (str "Unexpected arguments. " (clojure.string/join "." path) " of type " (:type field) " does not take any arguments but called with: " (clojure.string/join ", " query-args) ".")))
        (= (count query-args) 0) (throw (Exception. (str "Missing arguments. " (clojure.string/join "." path) " of type " (:type field) " requires arguments of " (clojure.string/join ", " field-args) " but none provided.")))
        :else (throw (Exception. (str "Wrong number of arguments. " (clojure.string/join "." path) " of type " (:type field) " requires " (count field-args) " arguments of " (clojure.string/join ", " field-args) " but called with " (clojure.string/join ", " query-args) "."))))
      (if (seq field-args)
        (update-in query [:args] (partial mapv
                                          (fn [field-arg query-arg]
                                            (try
                                              (coerce-argument field-arg query-arg)
                                              (catch Exception e
                                                (throw (Exception. (str "Invalid argument. Could not parse '" query-arg "' to type " field-arg " at "  (clojure.string/join "." path) "."))))))
                                          field-args))
        query))))

(defn validate-ids [query graph field path]
  (let [field-id (:id field)
        query-ids (:ids query)]
    (cond
      (nil? field-id) (if (seq query-ids)
                        (throw (Exception. (str "Invalid ids. " (name (:name field)) " does not take ids but was called with ids: " (clojure.string/join ", " query-ids))))
                        query)
      (not (coll? field-id)) (condp = (count query-ids)
                               0 (throw (Exception. (str "Missing id. " (name (:name field)) " takes a single id of type " (:id field) " but none was provided." )))
                               1 query
                               (throw (Exception. (str "Wrong number of ids. " (name (:name field)) " takes a single id of type " (:id field) " but called with " (clojure.string/join ", " query-ids)))))
      (coll? field-id) (condp = (count query-ids)
                         0 (throw (Exception. (str "Missing id. " (name (:name field)) " takes ids of type " (:id field) " but none was provided." )))
                         query))))

(defn validate-calls [query graph field path]
  (let [query-calls (:calls query)
        node-calls (get-in graph [:nodes (:type field) :calls])]
    (cond
      (empty? query-calls) query
      (and (empty? node-calls) (seq query-calls)) (throw (Exception. (str "Invalid calls. " (clojure.string/join "." path) " of type " (:type field) " does not take calls but was called with " (clojure.string/join ", " (map (comp name :name) query-calls)) ".")))
      :else (update-in query [:calls] (partial mapv (fn [call]
                                                      (if (contains? node-calls (:name call))
                                                        (let [query-args (:args call)
                                                              node-args (:args (node-calls (:name call)))]
                                                          (if (not= (count node-args) (count query-args))
                                                            (cond
                                                              (= (count node-args) 0) (throw (Exception. (str "Unexected arguments. Call " (name (:name call)) " at " (clojure.string/join "." path) " of type " (:type field) " does not take arguments, but called with " (clojure.string/join ", " query-args))))
                                                              (= (count query-args) 0) (throw (Exception. (str "Missing arguments. Call " (name (:name call)) " at " (clojure.string/join "." path) " of type " (:type field) " requires " (count node-args) " arguments of types " (clojure.string/join ", " node-args) " but was called with no arguments.")))
                                                              :else (throw (Exception. (str "Wrong number of arguments. Call " (name (:name call)) " at " (clojure.string/join "." path) " of type " (:type field) " requires " (count node-args) " arguments of types " (clojure.string/join ", " node-args) " but was called with " (clojure.string/join ", " query-args) "."))))
                                                            (if (seq query-args)
                                                              (update-in call [:args] (partial mapv
                                                                                               (fn [node-arg query-arg]
                                                                                                 (try
                                                                                                   (coerce-argument node-arg query-arg)
                                                                                                   (catch Exception e
                                                                                                     (throw (Exception. (str "Invalid arguments. Call " (name (:name call)) " at " (clojure.string/join "." path) " expects " (count node-args) " arguments of types " (clojure.string/join ", " node-args) " but could not parse " query-arg " to type " node-arg "."))))))
                                                                                               node-args))
                                                              call)))
                                                        (throw (Exception. (str "Unknown call. " (clojure.string/join "." path) " of type " (:type field) " does not have a call named " (name (:name call)) ". Supported calls are: " (clojure.string/join ", " (map name (keys node-calls))) "."))))))))))

(defn validate-query
  ([query graph]
   (if (insta/failure? query)
     (throw (Exception. (pr-str (insta/get-failure query))))
     (if-let [root (get-in graph [:roots (:name query)])]
       (walk-query validate-query query graph root [])
       (throw (Exception. (str "Unknown root " (name (:name query)) ". Known roots: " (clojure.string/join ", " (map name (keys (:roots graph)))) "."))))))
  ([query graph field path]
   (reduce (fn [query validator]
             (validator query graph field path))
           query
           [validate-scalar validate-complex validate-arguments validate-ids validate-calls])))

(defn parse [graph string]
  (-> string
    query-parser
    query-transform
    (validate-query graph)))
