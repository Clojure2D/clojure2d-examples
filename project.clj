(defproject clojure2d-examples "1.4.3"
  :description "Examples for Clojure2d library"
  :url "https://github.com/Clojure2D/clojure2d-examples"
  :license {:name "The Unlicense"
            :url "http://unlicense.org"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [sicmutils/sicmutils "0.21.1"]
                 [com.lambdaisland/cljbox2d "0.4.19"]
                 [clojure2d "1.4.4-SNAPSHOT"]]  
  :repl-options {:timeout 120000}
  :target-path "target/%s")
