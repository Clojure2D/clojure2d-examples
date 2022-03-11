;; https://github.com/lambdaisland/cljbox2d/blob/main/src/lambdaisland/cljbox2d/demo/pyramid.clj
(ns box2d.pyramid
  (:require [lambdaisland.cljbox2d :as b]
            [box2d.common :as bq]
            [clojure2d.core :as c2d]))

(def world (b/world 0 10))

(defn setup [_ _]
  (-> world
      (b/populate [{:fixtures [{:shape [:edge [-10 9.5] [20 9.5]]}]}])
      (b/populate
       (for [i (range 20)
             j (range i 20)]
         {:type :dynamic
          :position [(+ 6 (* i 0.5625) (* j -0.25))
                     (+ -3 (* j 0.6))]
          :fixtures [{:shape [:rect 0.4 0.4]}]})))

  (b/set-viewport! -2 -3 70))

(defn draw
  [canvas window _ _]
  (b/step-world world)
  (locking window (-> canvas
                      (c2d/set-background :white)
                      (c2d/set-color :black)
                      (c2d/set-stroke 5.0)
                      (bq/draw! world))))

(def window (c2d/show-window {:canvas (c2d/canvas 1200 1000)
                            :draw-fn draw
                            :setup setup
                            :window-name "Pyramid"}))

(defmethod c2d/key-pressed ["Pyramid" \space] [_ _] 
  (locking window (c2d/save (c2d/resize (c2d/get-image window) 600 500) "results/box2d/pyramid.jpg")))

