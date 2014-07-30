(defproject clojure.math "0.1.0-SNAPSHOT"
  :description ""
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/math.combinatorics "0.0.8"]
                 [org.apache.commons/commons-math3 "3.3"]]
  :main ^:skip-aot clojure.math
  :profiles {:uberjar {:aot :all}})
