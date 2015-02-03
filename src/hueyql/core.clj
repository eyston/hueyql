(ns hueyql.core
  (:require [hueyql.database :as database]
            [hueyql.graph.core :as graph]
            [hueyql.graph.datomic]
            [hueyql.user]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.util.response :as resp]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-params wrap-json-body]]
            [ring.middleware.stacktrace :refer [wrap-stacktrace]]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [datomic.api :as d]
            [cheshire.core :as json])
  (:gen-class))


(def current-user-id (atom 0))

(defn handle-graph [roots]
  (let [conn (d/connect database/uri)
        db (d/db conn)
        user (d/entity db @current-user-id)
        context {:db db :user user}
        roots (graph/parse-roots roots)]
    (resp/response {"result" (into [] (graph/hydrate roots context))})))

(defroutes app
  (GET "/" [] (resp/resource-response "index.html" {:root "public/www"}))
  (GET "/api/graph" [roots] (handle-graph (json/parse-string roots)))
  (POST "/api/graph" request (handle-graph (:body request)))
  (route/resources "/" {:root "/public/www/"})
  (route/not-found "not found"))

;; (def server (atom {}))
;; (reset! server (run-jetty (-> app wrap-params wrap-json-body wrap-json-response wrap-stacktrace) {:port (Integer. (or (System/getenv "PORT") "8080")) :join? false}))
;; (.stop @server)

(defn start []
  (run-jetty (-> app wrap-params wrap-json-body wrap-json-response) {:port (Integer. (or (System/getenv "PORT") "8080")) :join? false}))

(defn -main []
  (database/init)
  (reset! current-user-id (rand-nth (d/q '[:find [?e ...]
                                           :where
                                           [?e :user/first-name]]
                                         (d/db (d/connect database/uri)))))
  (start))
