(ns examples.NOC.ch08.simple-lsystem-8-6
  (:require [clojure2d.core :refer :all]))

(def cnvs (canvas 200 200))
(def window (show-window {:canvas cnvs
                          :window-name "Simple L-system 8_6"
                          :state {:string "A"
                                  :generation 0}}))

(with-canvas-> cnvs
  (set-background :white)
  (set-color :black)
  (text "Click mouse to generate" 10 (- (height cnvs) 20)))

(defmethod mouse-event [(:window-name window) :mouse-pressed] [_ {:keys [string generation] :as state}]
  (println (str "Generatation " generation ": " string))
  {:string (reduce #(str %1 (if (= %2 \A) "AB" "A")) "" string)
   :generation (inc generation)})
