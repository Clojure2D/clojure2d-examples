(defproject clojure2d-examples "1.4.3"
  :description "Examples for Clojure2d library"
  :url "https://github.com/Clojure2D/clojure2d-examples"
  :license {:name "The Unlicense"
            :url "http://unlicense.org"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 ;; [net.littleredcomputer/sicmutils "0.12.2-SNAPSHOT"]
                 [sicmutils/sicmutils "0.17.0"]
                 [clojure2d "1.4.3"]]  
  :repl-options {:timeout 120000}
  :target-path "target/%s"
  :jvm-opts ["-Xmx4096M"])
