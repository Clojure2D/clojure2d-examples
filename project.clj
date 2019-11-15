(defproject clojure2d-examples "1.3.0-alpha1"
  :description "Examples for Clojure2d library"
  :url "https://github.com/Clojure2D/clojure2d-examples"
  :license {:name "The Unlicense"
            :url "http://unlicense.org"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [net.littleredcomputer/sicmutils "0.12.0"]
                 [generateme/fastmath "1.5.0-alpha1"]
                 [clojure2d "1.3.0-alpha1"]]
  
  ;; For WINDOWS please uncomment two following lines
  ;; :exclusions [[ml.dmlc/xgboost4j] [asm]]
  ;; :resource-paths ["resources/" "lib/xgboost4j-0.81-criteo-20180821_2.11-win64.jar"]
  
  :repl-options {:timeout 120000}
  :target-path "target/%s"
  :jvm-opts ["-Xmx4096M"])
