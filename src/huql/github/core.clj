(ns huql.github.core
  (:require [clojure.core.cache :as cache]
            [clj-http.client :as client]
            [clj-http.links :as links]
            [aleph.http :as http]
            [byte-streams :as bs]
            [cheshire.core :as json]
            [manifold.deferred :as d]
            [clj-time.format :as f]
            [schema.core :as s]
            [environ.core :refer [env]]))

(def LIMIT 10)

(defonce CACHE (atom (cache/ttl-cache-factory {} :ttl 3600000)))

(defn http-get
  ([url]
   (http-get url nil))
  ([url options]
   (prn [:get url options])
   (let [ckey [url options]
         c (if (cache/has? @CACHE ckey)
             (swap! CACHE (fn [c] (cache/hit c ckey)))
             (let [response (:body (client/get url (merge {:as :json :headers {"Authorization" (str "token " (env :github-token))}} options)))]
               (swap! CACHE (fn [c] (cache/miss c ckey response)))))]
     (get c ckey))))

(defn paged-http-get
  ([url]
   (paged-http-get url 1))
  ([url page]
   (lazy-seq (let [resources (http-get url {:query-params {"page" page}})]
               (if (seq resources)
                 (lazy-cat resources (paged-http-get url (inc page)))
                 resources)))))

(defn get-resource [url]
  (http-get url))

(defn get-resources [url]
  (paged-http-get url))

(defn get-resources-count [url]
  (let [response (client/get url {:as :json :query-params {:per_page 1}})]
    (if-let [last-page (get-in response [:links :last :href])]
      (or (Integer/parseInt (second (re-find #"[\&\?]page=(\d+)?" last-page))) (count (:body response)))
      (count (:body response)))))

;; via https://github.com/dakrone/clj-http/blob/master/src/clj_http/links.clj -- just added let-flow
(defn wrap-links
  [client]
  (fn [request]
    (d/let-flow [response (client request)]
                (if-let [link-headers (get-in response [:headers "link"])]
                  (let [link-headers (if (coll? link-headers)
                                       link-headers
                                       [link-headers])]
                    (assoc response
                           :links
                           (apply merge (for [link-header link-headers]
                                          (links/read-link-headers link-header)))))
                  response))))

(defn get-resources-count-d [url]
  (d/let-flow [response (http/get url {:middleware wrap-links :headers {"User-Agent" "eyston" "Authorization" (str "token " (env :github-token))} :query-params {:per_page 1}})]
              (if-let [last-page (get-in response [:links :last :href])]
                (or (Integer/parseInt (second (re-find #"[\&\?]page=(\d+)?" last-page))) (count (:body response)))
                (-> response
                  :body
                  bs/to-string
                  (json/parse-string true)
                  count))))

(def DateTime org.joda.time.DateTime)


(declare url->user)
(declare url->users)
(declare url->organization)
(declare url->organization-summaries)
(declare url->organizations)
(declare url->commit)
(declare url->commits)
(declare url->repository)
(declare url->repositories)

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

(defn url->commit [url]
  (-> (get-resource url)
    (->> (s/validate CommitSchema))))

(defn url->commits [url]
  (->> (get-resources url)
    (take LIMIT)
    (map :url)
    (map url->commit)))

(defn commit-author [commit]
  (let [url (get-in commit [:author :url])]
    (url->user url)))

(defn commit-committer [commit]
  (let [url (get-in commit [:committer :url])]
    (url->user url)))


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

(defn parse-user [raw]
  (-> raw
    (update-in [:created_at] f/parse)
    (update-in [:updated_at] f/parse)
    (->> (s/validate UserSchema))))

(defn url->user [url]
  (-> (get-resource url)
    parse-user))

(defn login->user [login]
  (url->user (str "https://api.github.com/users/" login)))

(defn login->user-d [login]
  (let [url (str "https://api.github.com/users/" login)
        org (http/get url {:headers {"User-Agent" "eyston" "Authorization" (str "token " (env :github-token))}})]
    (d/chain org
             :body
             bs/to-string
             #(json/parse-string % true)
             parse-user)))

(defn url->users [url]
  (->> (get-resources url)
    (take LIMIT)
    (map :url)
    (map url->user)))

(defn user-organizations [user]
  (url->organizations (:organizations_url user)))

(defn user-followers [user]
  (url->users (:followers_url user)))


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

(defn url->organization [url]
  (-> (get-resource url)
    (update-in [:created_at] f/parse)
    (update-in [:updated_at] f/parse)
    (->> (s/validate OrganizationSchema))))

(defn url->organization-d [url]
  (let [org (http/get url {:headers {"User-Agent" "eyston" "Authorization" (str "token " (env :github-token))}})]
    (d/chain org
             :body
             bs/to-string
             #(json/parse-string % true))))

(defn name->organization [name]
  (url->organization (str "https://api.github.com/orgs/" name)))

(defn url->organization-summaries [url]
  (->> (get-resources url)
    (take LIMIT)))

(defn url->organizations [url]
  (->> (get-resources url)
    (take LIMIT)
    (map :url)
    (map url->organization)))

(defn organization-repositories [organization]
  (url->repositories (:repos_url organization)))


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

(defn url->repository [url]
  (-> (get-resource url)
    (update-in [:created_at] f/parse)
    (update-in [:updated_at] f/parse)
    (->> (s/validate RepositorySchema))))

(defn full-name->repository [full-name]
  (url->repository (str "https://api.github.com/repos/" full-name)))

(defn url->repositories [url]
  (->> (get-resources url)
    (take LIMIT)
    (map :url)
    (map url->repository)))

(defn repository-commits [repository]
  (let [rfc-url (:commits_url repository)
        url (clojure.string/replace rfc-url #"\{\/sha\}" "")]
    (url->commits url)))
