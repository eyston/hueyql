(ns huql.core
  (:require [huql.graph.core :as graph]
            [huql.graph.github :as gh]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.util.response :as resp]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.stacktrace :refer [wrap-stacktrace]]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [cheshire.core :as json])
  (:gen-class))

(defroutes app
  (GET "/" [] (resp/resource-response "index.html" {:root "public/www"}))
  (GET "/api/graph" [query profiled] (try
                                       (let [result (if profiled
                                                      (graph/run-profiled gh/github-graph query)
                                                      (graph/run gh/github-graph query))]
                                         (resp/response (json/generate-string result)))
                                       (catch Exception e {:status 500
                                                           :body (json/generate-string {:error (.getMessage e)})})))
  (route/resources "/" {:root "/public/www/"})
  (route/not-found "not found"))

(comment
  (def server (atom nil))

  (swap! server (fn [server]
                  (when server
                    (.stop server))
                  (run-jetty (-> app wrap-params) {:port (Integer. (or (System/getenv "PORT") "8080")) :join? false})))

  (swap! server (fn [server]
                  (when server
                    (.stop server))))
  )

(defn start []
  (run-jetty (-> app wrap-params) {:port (Integer. (or (System/getenv "PORT") "8080")) :join? false}))

(defn -main []
  (start))
