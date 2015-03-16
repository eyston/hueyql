(ns huql.graph.github
  (:require [huql.github.core :as gh]
            [huql.github.client :as client]
            [huql.graph.core :as graph]
            [clj-time.format :as f]
            [clj-time.core :as t]
            [manifold.stream :as stream]))

(defn first-call [count]
  (take count))

(defn after-call [accessor]
  (fn [cursor]
    (comp (drop-while (fn [item] (not= (accessor item) cursor)))
          (drop 1))))

(defn starts-with [accessor]
  (fn [s]
    (filter (fn [item] (.startsWith (accessor item) s)))))

(defn resource-exec [& calls]
  (let [calls (when calls (apply assoc (conj calls {})))]
    (fn [rs]
      (let [resources (stream/->source (client/resources (:url rs)))
            xf (apply comp (map (fn [call]
                                  (apply (calls (:name call)) (:args call)))
                                (:calls rs)))]
        (stream/transform xf resources)))))

(defn base-resource-exec [& calls]
  (apply resource-exec (concat [:first first-call :after (after-call :id)] calls)))

(defn resource-relation
  ([url-key]
   (resource-relation url-key nil))
  ([url-key count-key]
   (fn [parent & calls]
     (merge {:url (parent url-key)
             :calls calls}
            (when count-key
              {:count (parent count-key)})))))

(defn resource-count-exec [rs]
  (or (:count rs) (client/resources-count (:url rs))))


;;;; Organizations

(def OrganizationRoot (graph/root :Organization 'Organization gh/name->organization :id 'string))
(def OrganizationsRoot (graph/root :Organizations 'Organization gh/name->organization :id ['string]))

(graph/defnode Organization
  (field :login 'string :login)
  (field :id 'integer :id)
  (field :name 'string :name)
  (field :user 'User :user)
  (field :repositories 'Repositories (resource-relation :repos_url :public_repos)))

(graph/defnode Organizations
  (call :first :args ['integer])
  (call :after :args ['integer])
  (field :count 'integer resource-count-exec)
  (field :edges 'OrganizationEdge (base-resource-exec)
         :cardinality :many))

(graph/defnode OrganizationEdge
  (field :cursor 'integer :id)
  (field :node 'Organization (fn [summary] (gh/url->organization (:url summary)))))

;;;; Repositories

(graph/defnode Repositories
  (call :first :args ['integer])
  (call :after :args ['integer])
  (call :starts_with :args ['string])
  (field :count 'integer resource-count-exec)
  (field :edges 'RepositoryEdge (base-resource-exec :starts_with (starts-with :name))
         :cardinality :many))

(graph/defnode RepositoryEdge
  (field :cursor 'integer :id)
  (field :node 'Repository (fn [summary] (gh/url->repository (:url summary)))))

(graph/defnode Repository
  (field :id 'integer :id)
  (field :name 'string :name)
  (field :full_name 'string :full_name)
  (field :description 'string :description)
  (field :created_at 'DateTime :created_at))


;;; User

(def UserRoot (graph/root :User 'User gh/login->user :id 'string))
(def UsersRoot (graph/root :Users 'User gh/login->user :id ['string]))

(graph/defnode User
  (field :login 'string :login)
  (field :id 'integer :id)
  (field :name 'string :name)
  (field :location 'string :location)
  (field :created_at 'DateTime :created_at)
  (field :updated_at 'DateTime :updated_at)
  (field :organization 'Organization :organization)
  (field :organizations 'Organizations (resource-relation :organizations_url)))


;;; DateTime

(graph/defnode DateTime
  (field :month 'integer t/month)
  (field :year 'integer t/year)
  (field :day 'integer t/day)
  (field :format 'string (fn [dt format-string]
                           (let [formatter (f/formatter format-string)]
                             (f/unparse formatter dt)))
         :args ['string]))

(def github-graph (-> graph/graph

                    ;; Organizations
                    (graph/add-root OrganizationRoot)
                    (graph/add-root OrganizationsRoot)
                    (graph/add-node Organization)
                    (graph/add-node Organizations)
                    (graph/add-node OrganizationEdge)

                    ;; Repositories
                    (graph/add-node Repository)
                    (graph/add-node Repositories)
                    (graph/add-node RepositoryEdge)

                    ;; Users
                    (graph/add-root UserRoot)
                    (graph/add-root UsersRoot)
                    (graph/add-node User)

                    ;; DateTime
                    (graph/add-node DateTime)))
