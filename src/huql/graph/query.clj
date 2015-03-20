(ns huql.graph.query
  (:require [instaparse.core :as insta]
            [clojure.set :as set]
            [clojure.string :refer [join]]))

(def parser
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

(def transform
  (partial insta/transform {:NAME (fn [name] [:name (keyword name)])
                            :IDS (fn [& args] [:ids (vec args)])
                            :ARGS (fn [& args] [:args (vec args)])
                            :ALIAS (fn [alias] [:alias (keyword alias)])
                            :CALLS (fn [& calls] [:calls (vec calls)])
                            :CALL (fn [name args] (into {} [name args]))
                            :FIELDS (fn [& fields] [:fields (vec fields)])
                            :FIELD (fn [& args] (into {} args))
                            :ROOT (fn [& args] (into {} args))}))

(defn parse [string]
  (-> string
    parser
    transform))

;; validations -- so fucking long and shitty to read

(defn name->string [item]
  (name (:name item)))

(defn path->string [path]
  (join "." path))

(defn parse-scalar [graph field path scalar-name value]
  (let [scalar (get-in graph [:types scalar-name])]
    (try
      ((:parse scalar) value)
      (catch Exception e
        (throw (Exception. (str "Invalid argument. " (path->string path) " could not parse '" value "' to type " scalar-name ".")))))))

(defn parse-arguments [graph field path query]
  (let [field-args (:args field)
        query-args (:args query)]
    (if (seq field-args)
      (update-in query [:args] (partial mapv (partial parse-scalar graph field path) field-args))
      query)))

(defn parse-call-arguments [graph type-call path call]
  (let [type-args (:args type-call)]
    (if (seq type-args)
      (update-in call [:args] (partial mapv (fn [type-arg call-arg]
                                              (let [scalar (get-in graph [:types type-arg])]
                                                (try
                                                  ((:parse scalar) call-arg)
                                                  (catch Exception e
                                                    (throw (Exception. (str "Invalid call argument. " (path->string path) " call " (name->string call) " could not parse '" call-arg "' to type " type-arg ".")))))))
                                       type-args))
      call)))

(defn validate-call-argument-count [type-call path call]
  (let [type-args (:args type-call)
        call-args (:args call)]
    (if (not= (count type-args) (count call-args))
      (cond
        (= (count type-args) 0) (throw (Exception. (str "Invalid call arguments. " (path->string path) " call " (name->string call) " takes no arguments but called with: " (join ", " call-args))))
        (= (count call-args) 0) (throw (Exception. (str "Invalid call arguments. " (path->string path) " call " (name->string call) " takes " (count type-args) " arguments of " (join ", " type-args) " but none were provided.")))
        :else (throw (Exception. (str "Invalid call arguments. " (path->string path) " call " (name->string call) " takes " (count type-args) " arguments of " (join ", " type-args) " but called with: " (join ", " call-args) "."))))
      call)))

(defn validate-call [graph type-calls path call]
  (if-let [type-call (type-calls (:name call))]
    (->> call
      (validate-call-argument-count type-call path)
      (parse-call-arguments graph type-call path))
    (throw (Exception. (str "Invalid call. " (path->string path) " call " (name->string call) " is not supported.  Valid calls are: " (join ", " (map name (keys type-calls))) ".")))))

(defn validate-field-calls [graph field path query]
  (let [query-calls (:calls query)]
    (if (seq query-calls)
      (let [type (get-in graph [:types (:type field)])
            type-calls (:calls type)]
        (if (empty? type-calls)
          (throw (Exception. (str "Invalid call. " (path->string path) " does not accept calls but called with: " (join ", " (map name->string query-calls)) ".")))
          (update-in query [:calls] (partial mapv (partial validate-call graph type-calls path)))))
      query)))

(defn parse-arguments [graph field path query]
  (let [field-args (:args field)
        query-args (:args query)]
    (if (seq field-args)
      (update-in query [:args] (partial mapv (partial parse-scalar graph field path) field-args))
      query)))

(defn validate-argument-count [graph field path query]
  (let [field-args (:args field)
        query-args (:args query)]
    (if (not= (count field-args) (count query-args))
      (cond
        (= (count field-args) 0) (throw (Exception. (str "Illegal arguments. " (path->string path) " does not take any arguments but called with: " (join ", " query-args) ".")))
        (= (count query-args) 0) (throw (Exception. (str "Illegal arguments. " (path->string path) " requires " (count field-args) " arguments of " (join ", " field-args) " but none were provided.")))
        :else (throw (Exception. (str "Illegal arguments. " (path->string path) " requires " (count field-args) " arguments of " (join ", " field-args) " but was called with " (join ", " query-args) "."))))
      query)))

(defn validate-field-arguments [graph field path query]
  (->> query
    (validate-argument-count graph field path)
    (parse-arguments graph field path)))

(defn validate-field-scalar [graph field path query]
  (let [fields (:fields query)]
    (if (seq fields)
      (throw (Exception. (str "Invalid fields. " (path->string path) " is a scalar which does not support fields, but was queried with fields: " (join ", " (map name->string fields)))))
      query)))

(defn validate-has-fields [graph field path query]
  (if (empty? (:fields query))
    (let [valid-fields (keys (get-in graph [:fields (:type field)]))]
      (throw (Exception. (str "Invalid fields. " (path->string path) " requires fields but was queried with none.  Valid fields are: " (join ", " (map name valid-fields))))))
    query))

(defn validate-known-fields [graph field path query]
  (let [type-name (:type field)
        type (get-in graph [:types type-name])
        type-fields (into #{} (keys (get-in graph [:fields type-name])))
        query-fields (into #{} (map :name (:fields query)))
        unknown-fields (set/difference query-fields type-fields)]
    (if (seq unknown-fields)
      (throw (Exception. (str "Invalid fields. " (path->string path) " has unknown fields: " (join ", " (map name unknown-fields)) ".  Valid fields are: " (join ", " (map name type-fields)) ".")))
      query)))

(defn validate-fields [graph field path query]
  ;; TODO: add duplicate field detection
  (->> query
    (validate-has-fields graph field path)
    (validate-known-fields graph field path)))

(defn validate-field-complex [graph field path query]
  (validate-fields graph field path query))

(defn validate-field-type [graph field path query]
  (let [type (get-in graph [:types (:type field)])]
    (condp = (:type/type type)
      :scalar (validate-field-scalar graph field path query)
      :complex (validate-field-complex graph field path query))))

(defn validate-field [graph field path query]
  (->> query
    (validate-field-type graph field path)
    (validate-field-arguments graph field path)
    (validate-field-calls graph field path)))

(defn validate-root-ident [graph root path query]
  (let [query-ids (:ids query)]
    (if (seq query-ids)
      (throw (Exception. (str "Invalid ids. " (name->string root) " does not take ids but was called with: " (join ", " query-ids) ".")))
      query)))

(defn validate-root-one [graph root path query]
  (let [query-ids (:ids query)]
    (condp = (count query-ids)
      0 (throw (Exception. (str "Invalid id. " (name->string root) " takes a single id of type " (:id root) " but none was provided.")))
      1 query
      (throw (Exception. (str "Invalid ids. " (name->string root) " takes a single id of type " (:id root) " but was called with: " (join ", " query-ids) "."))))))

(defn validate-root-many [graph root path query]
  (let [query-ids (:ids query)]
    (if (seq query-ids)
      query
      (throw (Exception. (str "Invalid ids. " (name->string root) " takes 1 or more ids of type " (:id root) " but none was provided."))))))

(defn validate-root-ids [graph root path query]
  (condp = (:root/type root)
    :ident (validate-root-ident graph root path query)
    :one (validate-root-one graph root path query)
    :many (validate-root-many graph root path query)))

(defn validate-root [graph root path query]
  (->> query
    (validate-root-ids graph root path)
    (validate-fields graph root path)))

(defn walk-query [f graph field path query]
  (let [path (conj path (name (:name query)))]
    (if (seq (:fields query))
      (update-in (f graph field path query)
                 [:fields]
                 (partial mapv (fn [query]
                                 (let [field (get-in graph [:fields (:type field) (:name query)])]
                                   (walk-query f graph field path query)))))
      (f graph field path query))))

(defn validate
  ([graph query]
   (if (insta/failure? query)
     (throw (Exception. (pr-str (insta/get-failure query))))
     (if-let [root (get-in graph [:roots (:name query)])]
       (walk-query validate graph root [] query)
       (throw (Exception. (str "Unknown root " (name (:name query)) ". Known roots: " (clojure.string/join ", " (map name (keys (:roots graph)))) "."))))))
  ([graph field path query]
   (condp = (:graph/type field)
     :root (validate-root graph field path query)
     :field (validate-field graph field path query))))
