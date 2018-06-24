(ns GG.M.M-1-1-01
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [fastmath.vector :as v]))

(def ^:const wname "M_1_1_01")

(def cnvs (canvas 1024 256))
(def window (show-window {:canvas cnvs
                          :window-name wname
                          :state 42}))

(defn draw
  "Draw lines and dots."
  [seed]
  (let [xs (range 0 (width cnvs) 10)]
    
    (let [rng (r/rng :mersenne seed)
          p (for [x xs]
              (v/vec2 x (r/drandom rng 0 (height cnvs))))]
      (with-canvas-> cnvs
        (set-background :white)
        (set-color 0 130 164)
        (set-stroke 1 :round :round)
        (path p)))
    
    (let [rng (r/rng :mersenne seed)]
      (with-canvas [c cnvs]
        (set-color c :black)
        (set-stroke c 1)
        (doseq [x xs]
          (ellipse c x (r/drandom rng 0 (height cnvs)) 3 3)))))
  
  seed)

(defmethod mouse-event [wname :mouse-pressed] [_ s]
  (draw (r/irand 100000)))

(draw 42)
