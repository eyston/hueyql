(ns huql.github.client
  (:require [clj-http.links :as links]
            [aleph.http :as http]
            [byte-streams :as bs]
            [cheshire.core :as json]
            [manifold.deferred :as d]
            [manifold.stream :as s]
            [environ.core :refer [env]]
            [clojure.core.async :as async :refer [chan go-loop go >! <! close!]]))

(def LIMIT 100)

(defn- deep-merge [& vals]
  (if (every? map? vals)
    (apply merge-with deep-merge vals)
    (last vals)))

;; via https://github.com/dakrone/clj-http/blob/master/src/clj_http/links.clj -- just added let-flow
(defn- wrap-links
  [client]
  (fn [request]
    (d/let-flow [response (client request)]
                (if-let [link-headers (get-in response [:headers "link"])]
                  (let [link-headers (if (coll? link-headers)
                                       link-headers
                                       [link-headers])]
                    (assoc response
                           :links
                           (apply merge (for [link-header link-headers]
                                          (links/read-link-headers link-header)))))
                  response))))

(defn- bytes->json [bs]
  (-> bs
    bs/to-string
    (json/parse-string true)))

(defn http-get
  ([url]
   (http-get url nil))
  ([url options]
   (prn [:get url options])
   (let [out (chan)
         options (deep-merge options {:middleware wrap-links
                                      :headers {"User-Agent" "HuQL"}
                                      :query-params {"client_id" (env :github-client-id)
                                                     "client_secret" (env :github-client-secret)}})]
     (-> (http/get url options)
       (d/chain (fn [x]
                  (go
                    (>! out (update-in x [:body] bytes->json)))))
       (d/catch (fn [e]
                  (go (>! out e))))
       (d/finally #(close! out)))
     out)))

(defn resource [url]
  (go
    (:body (<! (http-get url)))))

(defn paged-resources
  ([url]
   (paged-resources url 1))
  ([url page]
   (let [out (chan)]
     (go-loop [page page]
       (let [response (<! (http-get url {:query-params {"page" page}}))
             resources (:body response)]
         (if (seq resources)
           (let [paged-resources (map (fn [r] {:resource r :page page}) resources)]
             (<! (async/onto-chan out paged-resources false))
             (recur (inc page)))
           (close! out))))
     (async/pipe out (chan 1 (take LIMIT))))))

(defn resources
  ([url]
   (resources url 1))
  ([url page]
   (async/pipe (paged-resources url page) (chan 1 (map :resource)))))

(defn resources-count [url]
  (go
    (let [response (<! (http-get url {:query-params {"per_page" 1}}))]
      (if-let [last-page (get-in response [:links :last :href])]
        (or (Integer/parseInt (second (re-find #"[\&\?]page=(\d+)?" last-page)))
            (count (:body response)))
        (count (:body response))))))
