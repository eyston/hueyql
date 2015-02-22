(ns huql.graph.github
  (:require [huql.github.core :as gh]
            [huql.graph.core :as graph]
            [schema.core :as s]
            [plumbing.map :as pm]
            [clj-time.format :as f]
            [clj-time.core :as t]
            [cheshire.core :as json]))

(def OrganizationsRoot
  {:key :Organizations
   :type 'Organization
   :roots (fn [query]
            (let [fields (:fields query)
                  names (:args query)]
              (into {} (map (fn [name]
                              [name {:query fields
                                     :input name}])))))
   :executor gh/name->organization})

(def OrganizationRoot
  {:key :Organization
   :type 'Organization
   :roots (fn [query]
            (let [name (first (:args query))]
              {name name}))
   :args (graph/tuple [s/Str 'name])
   :executor gh/name->organization})


(graph/defnode Organization
  (field :login 'string :login)
  (field :id 'integer :id)
  (field :name 'string :name)
  (field :repositories 'Repositories (fn [org]
                                       {:url (:repos_url org)
                                        :count (:public_repos org)})))

(def RepositoryRoot
  {:key :Repository
   :type 'Repository
   :roots (fn [query]
            (let [full-name (first (:args query))]
              {full-name full-name}))
   :args (graph/tuple [s/Str 'name])
   :executor gh/full-name->repository})


(graph/defnode Repositories
  (field :count 'integer :count)
  (field :edges 'RepositoryEdge (fn [rs]
                                  (gh/url->repositories (:url rs)))
         :cardinality :many))

(graph/defnode RepositoryEdge
  (field :cursor 'integer :id)
  (field :node 'Repository identity))

(graph/defnode Repository
  (field :id 'integer :id)
  (field :name 'string :name)
  (field :full_name 'string :full_name)
  (field :description 'string :description)
  (field :created_at 'DateTime :created_at))

(graph/defnode DateTime
  (field :month 'integer t/month)
  (field :year 'integer t/year)
  (field :day 'integer t/day)
  (field :format 'string (fn [dt format-string]
                           (let [formatter (f/formatter format-string)]
                             (f/unparse formatter dt)))
         :args [s/Str]))

(def UserRoot
  {:key :User
   :type 'User
   :roots (fn [query]
            (let [login (first (:args query))]
              {login login}))
   :args (graph/tuple [s/Str 'name])
   :executor gh/login->user-d})

(def UsersRoot
  {:key :Users
   :type 'User
   :roots (fn [query]
            (let [logins (:args query)]
              (into {} (map (fn [login]
                              [login login])
                            logins))))
   :args [s/Str]
   :executor gh/login->user-d})


(graph/defnode User
  (field :login 'string :login)
  (field :id 'integer :id)
  (field :name 'string :name)
  (field :location 'string :location)
  (field :created_at 'DateTime :created_at)
  (field :updated_at 'DateTime :updated_at)
  (field :organizations 'Organizations (fn [user]
                                         {:url (:organizations_url user)})))

(graph/defnode Organizations
  (field :count 'integer (fn [os] (gh/get-resources-count-d (:url os))))
  (field :edges 'OrganizationEdge (fn [os]
                                    ;; (vec (gh/url->organizations (:url os))))
                                    (vec (gh/url->organization-summaries (:url os))))
         :cardinality :many))

(graph/defnode OrganizationEdge
  (field :cursor 'integer :id)
  (field :node 'Organization (fn [summary] (gh/url->organization-d (:url summary)))))

(def github-graph (-> graph/graph
                    (graph/add-root OrganizationRoot)
                    (graph/add-root UserRoot)
                    (graph/add-root UsersRoot)
                    (graph/add-node Organization)
                    (graph/add-node Organizations)
                    (graph/add-node OrganizationEdge)
                    (graph/add-root RepositoryRoot)
                    (graph/add-node Repository)
                    (graph/add-node Repositories)
                    (graph/add-node RepositoryEdge)
                    (graph/add-node User)
                    (graph/add-node DateTime)))
