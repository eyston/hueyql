(ns huql.github.core
  (:require [huql.github.client :as client]
            [manifold.deferred :as d]
            [manifold.stream :as stream]
            [clj-time.format :as f]
            [schema.core :as s]
            [schema.coerce :as coerce]
            [schema.macros :as sm]
            [schema.utils :as su]
            [clojure.edn :as edn]))

(def DateTime org.joda.time.DateTime)

(defn string->datetime [ds]
  (or (f/parse ds) ds))

(def coercions (merge coerce/+string-coercions+
                      {DateTime string->datetime}))

(defn coercion-matcher [schema]
  (or (coercions schema)
      (coerce/keyword-enum-matcher schema)))

(defn parser [schema]
  (let [coercer (coerce/coercer schema coercion-matcher)]
    (fn [value]
      (if (nil? value) ;; let nils skip validation
        value
        (let [result (coercer value)]
          (if (su/error? result)
            (sm/error! (su/format* "Value does not match schema: %s" (pr-str result))
                       {:schema schema :value value :error result})
            result))))))

;;; COMMIT

(def CommitSchema
  {(s/optional-key :commit) {(s/optional-key :author) {(s/optional-key :date) (s/maybe DateTime)
                                                       s/Any s/Any}
                             (s/optional-key :committer) {(s/optional-key :date) (s/maybe DateTime)
                                                          s/Any s/Any}
                             s/Any s/Any}
   s/Any s/Any})

(def commit-parser (parser CommitSchema))

(defn url->commit [url]
  (d/chain (client/resource url)
           commit-parser))

;;; USER

(def UserSchema
  {(s/optional-key :created_at) (s/maybe DateTime)
   (s/optional-key :updated_at) (s/maybe DateTime)
   s/Any s/Any})

(def user-parser (parser UserSchema))


(defn url->user [url]
  (d/chain (client/resource url)
           user-parser))

(defn login->user [login]
  (url->user (str "https://api.github.com/users/" login)))

;;; ORGANIZATION

(def OrganizationSchema
  {(s/optional-key :created_at) (s/maybe DateTime)
   (s/optional-key :updated_at) (s/maybe DateTime)
   s/Any s/Any})

(def organization-parser (parser OrganizationSchema))

(defn url->organization [url]
  (d/chain (client/resource url)
           organization-parser))

(defn name->organization [name]
  (url->organization (str "https://api.github.com/orgs/" name)))


;;; REPOSITORY

(def RepositorySchema
  {(s/optional-key :created_at) (s/maybe DateTime)
   (s/optional-key :updated_at) (s/maybe DateTime)
   (s/optional-key :pushed_at) (s/maybe DateTime)
   s/Any s/Any})

(def repository-parser (parser RepositorySchema))

(defn url->repository [url]
  (d/chain (client/resource url)
           repository-parser))

(defn full-name->repository [full-name]
  (url->repository (str "https://api.github.com/repos/" full-name)))


