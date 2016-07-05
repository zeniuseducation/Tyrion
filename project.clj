(defproject tyrion "0.1.9"
  :description "Small data analysis and machine learning framework for Clojure"
  :url "https://github.com/zeniuseducation/Tyrion"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [net.mikera/core.matrix "0.52.1"]
                 [net.mikera/vectorz-clj "0.44.1"]
                 [gorilla-plot "0.1.4"]
                 [com.taoensso/timbre "4.4.0"]]
  :plugins [[lein-gorilla "0.3.6"]
            [lein-cloverage "1.0.7-SNAPSHOT"]]
  :repositories [["releases" {:url   "http://clojars.org/repo"
                              :creds :gpg}]])
