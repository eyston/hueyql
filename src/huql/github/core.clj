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
      (let [result (coercer value)]
        (if (su/error? result)
          (sm/error! (su/format* "Value does not match schema: %s" (pr-str result))
                     {:schema schema :value value :error result})
          result)))))

;;; COMMIT

(def CommitSchema
  {:sha s/Str
   :commit {:message s/Str
            s/Any s/Any}
   :url s/Str
   :author {:url s/Str
            s/Any s/Any}
   :committer {:url s/Str
               s/Any s/Any}
   s/Any s/Any})

(def commit-parser
  (coerce/coercer CommitSchema coercion-matcher))

(defn url->commit [url]
  (d/chain (client/resource url)
           commit-parser))

;;; USER

(def UserSchema
  {:login s/Str
   :id s/Int
   (s/optional-key :name) s/Str
   :url s/Str
   (s/optional-key :location) (s/maybe s/Str)
   :organizations_url s/Str
   :followers_url s/Str
   :created_at DateTime
   :updated_at DateTime
   s/Any s/Any})

(def user-parser
  (coerce/coercer UserSchema coercion-matcher))

(defn url->user [url]
  (d/chain (client/resource url)
           user-parser))

(defn login->user [login]
  (url->user (str "https://api.github.com/users/" login)))

;;; ORGANIZATION

(def OrganizationSchema
  {:login s/Str
   :id s/Int
   :url s/Str
   (s/optional-key :name) s/Str
   :repos_url s/Str
   :public_repos s/Int
   :created_at DateTime
   :updated_at DateTime
   s/Any s/Any})

(def organization-parser (parser OrganizationSchema))

(defn url->organization [url]
  (d/chain (client/resource url)
           organization-parser))

(defn name->organization [name]
  (url->organization (str "https://api.github.com/orgs/" name)))


;;; REPOSITORY

(def RepositorySchema
  {:id s/Int
   :name s/Str
   :full_name s/Str
   :description s/Str
   :commits_url s/Str
   :created_at DateTime
   :updated_at DateTime
   s/Any s/Any})

(def repository-parser
  (coerce/coercer RepositorySchema coercion-matcher))

(defn url->repository [url]
  (d/chain (client/resource url)
           repository-parser))

(defn full-name->repository [full-name]
  (url->repository (str "https://api.github.com/repos/" full-name)))
