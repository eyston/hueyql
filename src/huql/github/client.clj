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
   (let [options (deep-merge options {:middleware wrap-links
                                      :headers {"User-Agent" "HuQL"}
                                      :query-params {"client_id" (env :github-client-id)
                                                     "client_secret" (env :github-client-secret)}})]
     
     (d/chain (http/get url options)
              (fn [x]
                (update-in x [:body] bytes->json))))))

(defn resource [url]
  (d/chain (http-get url)
           :body))

(defn deferred->chan [d]
  (let [c (chan)]
    (-> d
      (d/chain (fn [x] (go (>! c x))))
      (d/catch (fn [e] (go (>! c e)))))
    c))

(defn paged-resources
  ([url]
   (paged-resources url 1))
  ([url page]
   (let [out (chan)]
     (go-loop [page page]
       (let [response (<! (deferred->chan (http-get url {:query-params {"page" page}})))
             resources (:body response)]
         (if (seq resources)
           (let [paged-resources (map (fn [r] {:resource r :page page}) resources)]
             (<! (async/onto-chan out paged-resources false))
             (recur (inc page)))
           (close! out))))
     (async/pipe out (chan 1 (take LIMIT))))))

;;;; stream version -- eager fetches a few pages
;;;; I like this version better but worried about API rate limiting atm!
;; (defn paged-resources
;;   ([url]
;;    (paged-resources url 1))
;;   ([url page]
;;    (->> (iterate inc page)
;;      (map (fn [page]
;;             (d/chain (http-get url {:query-params {"page" page}})
;;                      :body)))
;;      s/->source
;;      s/realize-each
;;      (s/map s/->source)
;;      s/concat
;;      (s/transform (take LIMIT)))))

(defn resources
  ([url]
   (resources url 1))
  ([url page]
   (async/pipe (paged-resources url page) (chan 1 (map :resource)))))

(defn resources-count [url]
  (d/let-flow [response (http-get url {:query-params {"per_page" 1}})]
              (if-let [last-page (get-in response [:links :last :href])]
                (or (Integer/parseInt (second (re-find #"[\&\?]page=(\d+)?" last-page)))
                    (count (:body response)))
                (count (:body response)))))
