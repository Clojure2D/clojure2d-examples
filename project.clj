(defproject clojure2d-examples "1.4.0"
  :description "Examples for Clojure2d library"
  :url "https://github.com/Clojure2D/clojure2d-examples"
  :license {:name "The Unlicense"
            :url "http://unlicense.org"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [net.littleredcomputer/sicmutils "0.12.2-SNAPSHOT"]
                 [generateme/fastmath "2.0.3"]
                 [clojure2d "1.4.0"]]  
  :repl-options {:timeout 120000}
  :target-path "target/%s"
  :jvm-opts ["-Xmx4096M"])
