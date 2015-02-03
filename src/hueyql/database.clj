(ns hueyql.database
  (:require [datomic.api :as d]))

(def uri "datomic:mem://users")

(def schema [{:db/id #db/id[:db.part/db]
              :db/ident :user/first-name
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db.install/_attribute :db.part/db}
             {:db/id #db/id[:db.part/db]
              :db/ident :user/last-name
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db.install/_attribute :db.part/db}
             {:db/id #db/id[:db.part/db]
              :db/ident :user/profile-pic
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db.install/_attribute :db.part/db}
             {:db/id #db/id[:db.part/db]
              :db/ident :user/is-verified
              :db/valueType :db.type/boolean
              :db/cardinality :db.cardinality/one
              :db.install/_attribute :db.part/db}
             {:db/id #db/id[:db.part/db]
              :db/ident :user/friends
              :db/valueType :db.type/ref
              :db/cardinality :db.cardinality/many
              :db.install/_attribute :db.part/db}
             {:db/id #db/id[:db.part/db]
              :db/ident :user/best-friend
              :db/valueType :db.type/ref
              :db/cardinality :db.cardinality/one
              :db.install/_attribute :db.part/db}])


(defn first-name []
  (rand-nth ["James" "John" "Robert" "Michael" "William" "David" "Richard" "Charles" "Joseph" "Thomas" "Christopher" "Daniel"
             "Sophia" "Emma" "Olivia" "Isabella" "Ava" "Mia" "Emily" "Abigail" "Harper" "Tamara" "Carrie" "Lindsey"]))

(defn last-name []
  (rand-nth ["Smith" "Johnson" "Williams" "Brown" "Jones" "Miller" "Davis" "Garcia" "Rodriquez" "Wilson" "Martinez" "Anderson"]))

(defn color-hex []
  (rand-nth ["00" "33" "66" "99" "CC" "FF"]))

(defn profile-pic [first-name last-name]
  (let [color (str (color-hex) (color-hex) (color-hex))]
    (str "http://placehold.it/50/" color "/FFFFFF&text=" (first first-name) (first last-name))))

(defn is-verified []
  (not= 0 (rand-int 10)))

(defn gen-user []
  (let [first-name (first-name)
        last-name (last-name)]
    {:db/id (d/tempid :db.part/user)
     :user/first-name first-name
     :user/last-name last-name
     :user/is-verified (is-verified)
     :user/profile-pic (profile-pic first-name last-name)}))

(defn friends [user users]
  (-> #{}
    (into (map
           (fn [x] (rand-nth users))
           (range 0 (+ 10 (rand-int 50)))))
    (disj user)))

(defn init []
  (d/delete-database uri)
  (d/create-database uri)
  (let [conn (d/connect uri)]
    @(d/transact conn schema)
    @(d/transact conn (vec (map (fn [n] (gen-user)) (range 0 100))))
    @(d/transact conn (let [users (d/q '[:find [?e ...]
                                         :where
                                         [?e :user/first-name]]
                                       (d/db conn))]
                        (vec (mapcat (fn [user]
                                       (let [friends (friends user users)
                                             best-friend (first friends)]
                                         (into [[:db/add user :user/best-friend best-friend]]
                                               (map (fn [friend] [:db/add user :user/friends friend]) friends))))
                                     users))))
    nil))

