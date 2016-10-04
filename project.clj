(defproject martian-robots "0.1.0-SNAPSHOT"
  :description "Scaring martian robots"
  :url "https://github.com/lazydevorg/martian-robots"
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot martian-robots.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
