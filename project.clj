(defproject clojure2d-examples "0.1.0-SNAPSHOT"
  :description "Examples for Clojure2d library"
  :url "https://github.com/Clojure2D/clojure2d-examples"
  :license {:name "The Unlicense"
            :url "http://unlicense.org"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [net.littleredcomputer/sicmutils "0.10.0"]
                 [generateme/fastmath "0.1.2-SNAPSHOT"]
                 [clojure2d "0.1.0-SNAPSHOT"]]
  :repl-options {:timeout 120000}
  :target-path "target/%s"
  :jvm-opts ["-Xmx4096M"]
  :profiles {:dev {:plugins [[refactor-nrepl "2.4.0-SNAPSHOT"]
                             [cider/cider-nrepl "0.18.0-SNAPSHOT"]]}})
