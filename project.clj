(defproject huql "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [ring "1.3.2"]
                 [ring/ring-json "0.3.1"]
                 [compojure "1.3.1"]
                 [cheshire "5.4.0"]
                 [org.clojure/core.cache "0.6.4"]
                 [clj-http "1.0.1"]
                 [prismatic/schema "0.3.7"]
                 [environ "1.0.0"]]
  :min-lein-version "2.0.0"
  :main huql.core
  :plugins [[lein-environ "1.0.0"]])
