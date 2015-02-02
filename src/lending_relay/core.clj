(ns lending-relay.core
  (:require [lending-relay.database :as database]
            [lending-relay.user :as user]
            [lending-relay.graph :as graph]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.util.response :as resp]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-params wrap-json-body]]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [datomic.api :as d]))

(def current-user-id (atom 0))

(defroutes app
  (GET "/" [] (resp/resource-response "index.html" {:root "public/www"}))
  (POST "/api/graph" request
        (let [graph (get-in request [:body "graph"])
              graph (graph/parse :roots graph)
              conn (d/connect database/uri)
              db (d/db conn)
              context {:db db
                       :con conn
                       :current-user (d/entity db @current-user-id)}
              datoms (graph/datoms context graph)
              result (graph/hydrate context graph)]
          (resp/response {:graph graph
                          :data datoms
                          :result result})))
  (route/resources "/" {:root "/public/www/"})
  (route/not-found "not found"))

(defn start []
  (database/init)
  (reset! current-user-id (rand-nth (d/q '[:find [?e ...]
                                           :where
                                           [?e :user/first-name]]
                                         (d/db (d/connect database/uri)))))
  (run-jetty (-> app wrap-json-body wrap-json-response) {:port 8080 :daemon? true :join? false}))

(start)
