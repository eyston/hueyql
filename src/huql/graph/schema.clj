(ns huql.graph.github
  (:require [schema.core :as s]))

(def Commit
  {:sha s/Str
   :message s/Str})

(def User
  {:login s/Str
   :id s/Int
   :name s/Str
   :avatar_url s/Str
   :location s/Str})

(def CommitResource
  {:sha s/Str
   :commit {:author {:name s/Str}
            :message s/Str}
   :author {:login s/Str
            :id s/Int
            :avatar_url s/Str
            :url s/Str}})

(def UserResource
  {:login s/Str
   :id s/Int
   :url s/Str
   :name s/Str
   :location s/Str
   :avatar_url s/Str})

(def CommitRoot
  {:sha s/Str})

(def query
  {:sha s/Str
   :message s/Str
   :author {:name s/Str}})

(def CommitRelations
  {:author {:schema User
            :executors [:commit->author]}
   :committer {:schema User
               :executors [:commit->committer]}})

(def graph
  {:commit {:schema {}
            :relations {:author :user
                        :committer :user}}
   :user {:schema {}
          :relations {:repositories [:repository]}}
   :repository {:schema {}
                :relations {:most_recent_commit :commit}}
   :organization {:schema {}
                  :relations {:repositories [:repository]}}})

(def organization-root
  (select graph
          [:organization]
          [:organization :repositories]
          [:repository :most_recent_commit]
          [:repository :most_recent_commit :author]
          [:repository :most_recent_commit :committer]))

(def organization-root
  (select graph
          [:organization]
          [:organization :repositories]
          [:organization :repositories :filters :first]
          [:organization :repositories :filters :after]
          [:organization :repositories :most_recent_commit]
          [:organization :repositories :most_recent_commit :author]
          [:organization :repositories :most_recent_commit :committer]))

(def organization-root
  (select graph
          [:organization [:repositories [:filters [:first
                                                   :after]
                                         :most_recent_commit [:author
                                                              :comitter]]]]))


(def CommitResource->Commit
  (-> Commit
    (assoc :author (select-keys User [:login :id :name :avatar_url]))))

(def graph
  {CommitRoot {:edges [{:output CommitResource
                        :executor :url->commit-resource}]}
   CommitResource {:edges [{:output CommitResource->Commit
                            :executor :commit-resource->commit}]}
   CommitResource->Commit {:edges [{:output (-> CommitResource->Commit
                                              (assoc :author User))
                                    :executor :commit->author}]}})



(def graph
  {CommitRoot {:edges [{:output CommitResource
                        :executor :url->commit-resource}]}
   CommitResource {:edges [{:output (-> Commit (assoc :author (select-keys User [:login :id :name :avatar_url])))
                            :executor :commit-resource->commit}
                           {:output (-> Commit (assoc :author User))
                            :executor :commit-with-author}]}})

(def edges
  {:url->commit-resource {:in {:url s/Str} :out CommitResource}
   :url->author-resource {:in {:url s/Str} :out AuthorResource}
   :commit-resource->commit {:in CommitResource :out (-> Commit (assoc :author (select-keys User [:login :id :name :avatar_url])))}
   :commit-with-author {:in Commit :out (-> Commit (assoc :author User))}
   :user-resource->user {:in UserResource :out User}})
