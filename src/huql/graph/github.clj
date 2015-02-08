(ns huql.graph.github
  (:require [huql.graph.core :as graph]
            [clojure.core.cache :as cache]
            [clj-http.client :as client]
            [schema.core :as s]
            [environ.core :refer [env]]
            [clojure.string]
            [clojure.set :as set]))

(defonce CACHE (atom (cache/ttl-cache-factory {} :ttl 3600000)))

(defn http-get [url]
  (prn [:get url])
  (get (swap! CACHE (fn [c]
                      (if (cache/has? c url)
                        (cache/hit c url)
                        (cache/miss c url (:body (client/get url {:as :json :headers {"Authorization" (str "token " (env :github-token))}}))))))
       url))

(def PosInt (s/both s/Int (s/pred #(> % 0) 'positive?)))

(def Organization
  {:name s/Str
   :description s/Str
   :email s/Str})

(def Repository
  {:id s/Int
   :name s/Str
   :full_name s/Str
   :description s/Str
   :language s/Str
   :watchers s/Int
   :forks s/Int})

(def Commit
  {:sha s/Str
   :message s/Str})

(def Author
  {:login s/Str
   :id s/Int
   :name s/Str
   :avatar_url s/Str
   :location s/Str})


(def OrganizationRoot
  (let [author Author
        latest-commit (-> Commit
                        (assoc :author Author))
        repository (-> Repository
                     (assoc :latest_commit latest-commit))
        repositories (graph/collection-object
                      {:first {:count PosInt}
                       :after {:cursor PosInt}}
                      s/Int
                      repository)]
    (-> Organization
      (assoc :repositories repositories))))

(defn lazy-resources
  ([url]
   (lazy-resources url 1))
  ([url page]
   (lazy-seq (let [paged-url (str url (when (> page 1) (str "?page=" page)))
                   resources (http-get paged-url)]
               (if (seq resources)
                 (lazy-cat resources (lazy-resources url (inc page)))
                 resources)))))

(defrecord AuthorExecutor [url]
  graph/IExecutor
  (execute [_ schema]
    (http-get url)))

(defrecord CommitAuthorExecutor [author]
  graph/IExecutor
  (execute [_ schema]
    (let [required-keys (into #{} (keys schema))
          missing-keys (set/difference required-keys (into #{} (keys author)))
          url (:url author)]
      (if (and (seq missing-keys) url)
        (merge author (graph/execute (AuthorExecutor. url) schema))
        author))))

(defrecord LatestCommitExecutor [url]
  graph/IExecutor
  (execute [_ schema]
    (let [url (clojure.string/replace url #"\{\/sha\}" "")
          commits (http-get (str url "?per_page=1"))
          commit (first commits)]
      (-> commit
        (assoc :author (CommitAuthorExecutor. (merge (:author commit) (get-in commit [:commit :author]))))))))

(defrecord RepositoryExecutor [repo]
  graph/IExecutor
  (execute [_ schema]
    (-> repo
      (assoc :latest_commit (LatestCommitExecutor. (:commits_url repo))))))

(defrecord RepositoriesExecutor [url count]
  graph/IExecutor
  (execute [_ schema]
    (let [filters (:filters schema)]
      {:count count
       :filters filters
       :edges (graph/executor (fn []
                                (let [repos (lazy-resources url)
                                      repos (reduce (fn [repos filter-key]
                                                      (if-let [filter (get filters filter-key)]
                                                        (condp = filter-key
                                                          :first (take (:count filter) repos)
                                                          :after (let [cursor (:cursor filter)]
                                                                   (->> repos
                                                                     (drop-while #(not= cursor (:id %)))
                                                                     (drop 1))))
                                                        repos))
                                                    repos
                                                    [:after :first])]
                                  (mapv (fn [repo]
                                          {:cursor (:id repo)
                                           :node (RepositoryExecutor. repo)})
                                        repos))))})))

(defrecord OrganizationRootExecutor [name]
  graph/IExecutor
  (execute [_ schema]
    (let [org (http-get (str "https://api.github.com/orgs/" name))]
      (-> org
        (assoc :repositories (RepositoriesExecutor. (:repos_url org)
                                                    (:public_repos org)))))))

(graph/register :organization (fn [root call]
                                (let [[_ org] root]
                                  (graph/walk (graph/build OrganizationRoot call) (OrganizationRootExecutor. org)))))
