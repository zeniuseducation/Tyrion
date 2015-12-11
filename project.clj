(defproject tyrion "0.1.6"
  :description "Small data analysis and machine learning framework for Clojure"
  :url "https://github.com/zeniuseducation/Tyrion"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [net.mikera/core.matrix "0.47.0"]
                 [net.mikera/vectorz-clj "0.38.0"]
                 [gorilla-plot "0.1.3"]]
  :plugins [[lein-gorilla "0.3.5"]]
  :repositories [["releases" {:url   "http://clojars.org/repo"
                              :creds :gpg}]])
