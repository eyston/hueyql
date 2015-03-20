(ns huql.graph.github
  (:require [huql.github.core :as gh]
            [huql.github.client :as client]
            [huql.graph.core :as graph]
            [huql.graph.query :as query]
            [clj-time.format :as f]
            [clj-time.core :as t]
            [manifold.stream :as stream]))

;; helpers for github graph

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
     (let [url (parent url-key)
           url (clojure.string/replace url  #"\{.*\}" "")
           _ (prn [:url url])]
       (merge {:url url
               :calls calls}
              (when count-key
                {:count (parent count-key)}))))))

(defn resource-count-exec [rs]
  (or (:count rs) (client/resources-count (:url rs))))


;;;; Organizations

(def OrganizationRoot (graph/root :Organization 'Organization gh/name->organization :one 'string))

(def OrganizationsRoot (graph/root :Organizations 'Organization gh/name->organization :many 'string))

(def FacebookRoot (graph/root :Facebook 'Organization (fn [_] (gh/name->organization "facebook")) :ident))

(def OrganizationType (graph/type 'Organization
                                  [:field :login 'string :login]
                                  [:field :id 'integer :id]
                                  [:field :url 'string :url]
                                  [:field :repositories 'Repositories (resource-relation :repos_url :public_repos)]
                                  ;; TODO: events_url
                                  [:field :public_members 'Users (resource-relation :public_members_url)]
                                  [:field :avatar_url 'string :avatar_url]
                                  [:field :description 'string :description]
                                  [:field :name 'string :name]
                                  [:field :company 'string :company]
                                  [:field :blog 'string :blog]
                                  [:field :location 'string :location]
                                  [:field :email 'string :email]
                                  [:field :public_gists 'integer :public_gists]
                                  [:field :followers 'integer :followers]
                                  [:field :following 'integer :following]
                                  [:field :html_url 'string :html_url]
                                  [:field :updated_at 'DateTime :updated_at]
                                  [:field :created_at 'DateTime :created_at]
                                  [:field :type 'string :type]))

(def OrganizationsType (graph/type 'Organizations
                                   [:call :first ['integer]]
                                   [:call :after ['integer]]
                                   [:field :count 'integer resource-count-exec]
                                   [:field :edges 'OrganizationEdge (base-resource-exec)
                                    :cardinality :many]))

(def OrganizationEdgeType (graph/type 'OrganizationEdge
                                      [:field :cursor 'integer :id]
                                      [:field :node 'Organization (fn [summary] (gh/url->organization (:url summary)))]))


;;;; Repositories

(def RepositoryRoot (graph/root :Repository 'Repository gh/full-name->repository :one 'string))

(def RepositoriesRoot (graph/root :Repositories 'Repository gh/full-name->repository :many 'string))

(def RepositoryType (graph/type 'Repository
                                [:field :id 'integer :id]
                                [:field :name 'string :name]
                                [:field :full_name 'string :full_name]
                                [:field :owner 'Owner #(client/resource (get-in % [:owner :url]))]
                                [:field :private 'boolean :private]
                                [:field :html_url 'string :html_url]
                                [:field :description 'string :description]
                                [:field :fork 'boolean :fork]
                                [:field :url 'string :url]
                                [:field :forks 'Repositories (resource-relation :forks_url :forks_count)]
                                ;; TODO: keys_url -- no idea what this is
                                ;; TODO: collaborators_url -- seems to require auth
                                ;; TODO: teams_url -- auth
                                ;; TODO: hooks_url -- auth
                                ;; TODO: issue_events_url
                                ;; TODO: events_url
                                [:field :assignees 'Users (resource-relation :assignees_url)]
                                ;; TODO: branches_url
                                ;; TODO: tags_url
                                ;; TODO: blobs_url
                                ;; TODO: git_tags_url
                                ;; TODO: git_refs_url
                                ;; TODO: trees_url
                                ;; TODO: statuses_url
                                ;; TODO: languages_url
                                [:field :stargazers 'Users (resource-relation :stargazers_url :stargazers_count)]
                                [:field :contributors 'Users (resource-relation :contributors_url)]
                                [:field :subscribers 'Users (resource-relation :subscribers_url :subscribers_count)]
                                ;; TODO: subscription_url -- auth?
                                [:field :commits 'Commits (resource-relation :commits_url)]
                                ;; TODO: git_commits_url
                                ;; TODO: comments_url
                                ;; TODO: issue_comment_url
                                ;; TODO: conents_url
                                ;; TODO: compare_url
                                ;; TODO: merges_url
                                ;; TODO: archive_url
                                ;; TODO: downloads_url
                                ;; TODO: issues_url
                                ;; TODO: pulls_url
                                ;; TODO: milestones_url
                                ;; TODO: notifications_url
                                ;; TODO: labels_url
                                ;; TODO: releases_url
                                [:field :created_at 'DateTime :created_at]
                                [:field :updated_at 'DateTime :updated_at]
                                [:field :pushed_at 'DateTime :pushed_at]
                                [:field :git_url 'string :git_url]
                                [:field :ssh_url 'string :ssh_url]
                                [:field :clone_url 'string :clone_url]
                                [:field :svn_url 'string :svn_url]
                                [:field :homepage 'string :homepage]
                                [:field :size 'integer :size]
                                [:field :watchers 'integer :watchers]
                                [:field :language 'string :language]
                                [:field :has_issues 'boolean :has_issues]
                                [:field :has_downloads 'boolean :has_downloads]
                                [:field :has_wiki 'boolean :has_wiki]
                                [:field :has_pages 'boolean :has_pages]
                                [:field :mirror_url 'string :mirror_url]
                                [:field :open_issues 'integer :open_issues]
                                [:field :default_branch 'string :default_branch]
                                [:field :organization 'User (fn [repo] (gh/url->user (get-in repo [:organization :url])))] ;; is this always a user?
                                [:field :network_count 'integer :network_count]))

(def RepositoriesType (graph/type 'Repositories
                                  [:call :first ['integer]]
                                  [:call :after ['integer]]
                                  [:call :starts_with ['string]]
                                  [:field :count 'integer resource-count-exec]
                                  [:field :edges 'RepositoryEdge (base-resource-exec :starts_with (starts-with :name))
                                   :cardinality :many]))

(def RepositoryEdgeType (graph/type 'RepositoryEdge
                                    [:field :cursor 'integer :id]
                                    [:field :node 'Repository (fn [summary] (gh/url->repository (:url summary)))]))


;;; User

(def UserRoot (graph/root :User 'User gh/login->user :one 'string))

(def UsersRoot (graph/root :Users 'User gh/login->user :many 'string))

(def UserType (graph/type 'User
                          [:field :login 'string :login]
                          [:field :id 'integer :id]
                          [:field :avatar_url 'string :avatar_url]
                          [:field :gravatar_id 'string :gravatar_id]
                          [:field :url 'string :url]
                          [:field :html_url 'string :html_url]
                          [:field :organizations 'Organizations (resource-relation :organizations_url)]
                          [:field :followers 'Users (resource-relation :followers_url :followers)]
                          [:field :following 'Users (resource-relation :following_url :following)]
                          ;; TODO: gists_url / public_gists
                          [:field :starred 'Repositories (resource-relation :starred_url)]
                          [:field :subscriptions 'Repositories (resource-relation :subscriptions_url)]
                          [:field :repositories 'Repositories (resource-relation :repos_url :public_repos)]
                          ;; TODO: events_url
                          ;; TODO: received_events_url
                          [:field :type 'string :type]
                          [:field :site_admin 'boolean :site_admin]
                          [:field :name 'string :name]
                          [:field :company 'string :company]
                          [:field :blog 'string :blog]
                          [:field :location 'string :location]
                          [:field :email 'string :email]
                          [:field :hireable 'boolean :hireable]
                          [:field :bio 'string :bio]
                          [:field :created_at 'DateTime :created_at]
                          [:field :updated_at 'DateTime :updated_at]))

(def UsersType (graph/type 'Users
                           [:call :first ['integer]]
                           [:call :after ['integer]]
                           [:field :count 'integer resource-count-exec]
                           [:field :edges 'UserEdge (base-resource-exec)
                            :cardinality :many]))

(def UserEdgeType (graph/type 'UserEdge
                              [:field :cursor 'integer :id]
                              [:field :node 'User (fn [summary] (gh/url->user (:url summary)))]))

;;; Commit

(def CommitType (graph/type 'Commit
                            [:field :sha 'string :sha]
                            [:field :html_url 'string :html_url]
                            [:field :message 'string #(get-in % [:commit :message])]
                            [:field :author_date 'DateTime #(get-in % [:commit :author :date])]
                            [:field :commit_date 'DateTime #(get-in % [:commit :committer :date])]
                            ;; TODO: tree
                            ;; TODO: comments_url
                            ;; TODO: parents -- trees?
                            ;; TODO: files
                            [:field :author 'User #(gh/url->user (get-in % [:author :url]))]
                            [:field :committer 'User #(gh/url->user (get-in % [:committer :url]))]
                            [:field :additions 'integer #(get-in % [:stats :additions])]
                            [:field :deletions 'integer #(get-in % [:stats :deletions])]
                            [:field :total 'integer #(get-in % [:stats :total])]))
                            
(def CommitsType (graph/type 'Commits
                             [:call :first ['integer]]
                             [:call :after ['integer]]
                             ;; [:field :count 'integer resource-count-exec] ;; this returns 1 ...
                             [:field :edges 'CommitEdge (base-resource-exec)
                              :cardinality :many]))

(def CommitEdgeType (graph/type 'CommitEdge
                                [:field :cursor 'string :sha]
                                [:field :node 'Commit (fn [summary] (gh/url->commit (:url summary)))]))

;;; Owner
;; repos have owners -- either a user / org


(def OwnerType (graph/type 'Owner
                          [:field :login 'string :login]
                          [:field :id 'integer :id]
                          [:field :avatar_url 'string :avatar_url]
                          [:field :gravatar_id 'string :gravatar_id]
                          [:field :url 'string :url]
                          [:field :html_url 'string :html_url]
                          [:field :followers 'Users (resource-relation :followers_url)]
                          [:field :following 'Users (resource-relation :following_url)]
                          ;; TODO: gists_url / public_gists
                          [:field :starred 'Repositories (resource-relation :starred_url)]
                          [:field :subscriptions 'Repositories (resource-relation :subscriptions_url)]
                          [:field :organizations 'Organizations (resource-relation :organizations_url)]
                          [:field :repositories 'Repositories (resource-relation :repos_url :public_repos)]
                          ;; TODO: events_url
                          ;; TODO: received_events_url
                          [:field :type 'string :type]
                          [:field :site_admin 'boolean :site_admin]))

;;; Issues

;;; Events


;;; DateTime

(def DateTimeType (graph/type 'DateTime
                              [:field :month 'integer t/month]
                              [:field :year 'string t/year]
                              [:field :day 'string t/day]
                              [:field :format 'string (fn [dt format-string]
                                                        (let [formatter (f/formatter format-string)]
                                                          (f/unparse formatter dt)))
                               :args ['string]]))

(def github-graph (graph/->graph
                   OrganizationRoot
                   OrganizationsRoot
                   FacebookRoot
                   OrganizationType
                   OrganizationsType
                   OrganizationEdgeType

                   RepositoryRoot
                   RepositoriesRoot
                   RepositoryType
                   RepositoriesType
                   RepositoryEdgeType
                   
                   UserRoot
                   UsersRoot
                   UserType
                   UsersType
                   UserEdgeType

                   CommitType
                   CommitsType
                   CommitEdgeType

                   OwnerType

                   DateTimeType))
