(ns huql.core
  (:require [huql.graph.core :as graph]
            [huql.graph.github]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.util.response :as resp]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-params wrap-json-body]]
            [ring.middleware.stacktrace :refer [wrap-stacktrace]]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [cheshire.core :as json])
  (:gen-class))

;; sample call -- hard coded for now
(def call {:name nil
           :description nil
           :email nil
           :repositories {:count nil
                          :filters {:first {:count 20}
                                    :after {:cursor 4276357}}
                          :edges {:cursor nil
                                  :node {:id nil
                                         :name nil
                                         :full_name nil
                                         :description nil
                                         :latest_commit {:sha nil
                                                         :author {:name nil
                                                                  :avatar_url nil
                                                                  :location nil}}}}}})

(defroutes app
  (GET "/api/graph" [] (resp/response (graph/execute-roots {[:organization "facebook"] call})))
  (route/not-found "not found"))

(defn start []
  (run-jetty (-> app wrap-params wrap-json-body wrap-json-response) {:port (Integer. (or (System/getenv "PORT") "8080")) :join? false}))

(defn -main []
  (start))
