(defproject clojure2d-examples "1.5.0-SNAPSHOT"
  :description "Examples for Clojure2d library"
  :url "https://github.com/Clojure2D/clojure2d-examples"
  :license {:name "The Unlicense"
            :url "http://unlicense.org"}
  :plugins [[lein-tools-deps "0.4.5"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]}  
  :repl-options {:timeout 120000}
  :target-path "target/%s"
  :profiles {:eastwood {:plugins [[jonase/eastwood "1.4.0"]]
                        :eastwood {:add-linters [:performance :boxed-math]}}})
