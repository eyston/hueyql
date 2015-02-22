(defproject huql "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [ring "1.3.2"]
                 [compojure "1.3.1"]
                 [cheshire "5.4.0"]
                 [org.clojure/core.cache "0.6.4"]
                 [clj-http "1.0.1"]
                 [prismatic/schema "0.3.7"]
                 [prismatic/plumbing "0.3.7"]
                 [manifold "0.1.0-beta11"]
                 [aleph "0.4.0-beta3"]
                 [instaparse "1.3.5"]
                 [clj-time "0.9.0"]
                 [environ "1.0.0"]]
  :min-lein-version "2.0.0"
  :main huql.core
  :plugins [[lein-environ "1.0.0"]])
