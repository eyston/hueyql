(ns hueyql.user
  (:require [clojure.set :as set]
            [hueyql.graph.datomic :as graph]))

(defmethod graph/mapper :user/name [_ entity _]
  (str (:user/first-name entity) " " (:user/last-name entity)))

(defmethod graph/mapper :user/mutual-friends [_ entity {:keys [user]}]
  (set/intersection (:user/friends entity) (:user/friends user)))
