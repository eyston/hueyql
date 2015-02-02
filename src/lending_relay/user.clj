(ns lending-relay.user
  (:require [datomic.api :as d]
            [clojure.set :as set]
            [lending-relay.graph :as graph]))


(defn full-name [user]
  (str (:user/first-name user) " " (:user/last-name user)))

(defn mutual-friends [user1 user2]
  (let [friends1 (:user/friends user1 #{})
        friends2 (:user/friends user2 #{})]
    (set/union friends1 friends2)))

;;(ns-unmap *ns* 'mapper)

(defmethod graph/mapper :user/name [_ user _]
  (full-name user))

(defmethod graph/mapper :user/mutual-friends [_ user context]
  (let [current-user (:current-user context)]
    (mutual-friends user current-user)))

(defmethod graph/mapper :default [field user _]
  (get user field))


(defmethod graph/viewer :friends [_ options fields context]
  (let [db (:db context)
        current-user (:current-user context)
        count (get options :count 20)
        sort (get options :sort :user/last-name)
        entities (->> (d/q '[:find [?f ...]
                             :in $ ?user-id
                             :where
                             [?user-id :user/friends ?f]]
                           db (:db/id current-user))
                   (take count)
                   (map #(d/entity db %))
                   (sort-by sort))]
    (graph/ref->many context :friends :viewer entities fields)))

