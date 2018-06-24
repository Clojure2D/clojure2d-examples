(defproject clojure2d-examples "1.0.0-RC1"
  :description "Examples for Clojure2d library"
  :url "https://github.com/Clojure2D/clojure2d-examples"
  :license {:name "The Unlicense"
            :url "http://unlicense.org"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [net.littleredcomputer/sicmutils "0.10.0"]
                 [generateme/fastmath "1.0.1"]
                 [clojure2d "1.0.0-RC1"]
                 [org.apache.xmlgraphics/batik-transcoder "1.10"]]
  :repl-options {:timeout 120000}
  :target-path "target/%s"
  :jvm-opts ["-Xmx4096M"]
  :profiles {:dev {:plugins [[refactor-nrepl "2.4.0-SNAPSHOT"]
                             [cider/cider-nrepl "0.18.0-SNAPSHOT"]]}})
