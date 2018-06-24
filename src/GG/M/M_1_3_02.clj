(ns GG.M.M-1-3-02
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [clojure2d.pixels :as p]
            [clojure2d.color :as c]))

(def ^:const wname "M_1_3_02")

(def cnvs (canvas 512 512))
(def window (show-window {:canvas cnvs
                          :window-name wname
                          :state 0}))

(defn draw
  "Draw lines and dots."
  [seed]

  (let [rng (r/rng :well512a seed)
        filt (fn [_] (c/gray (r/irandom rng 255)))]
    (p/set-canvas-pixels! cnvs (p/filter-colors filt (p/to-pixels cnvs))))
  
  seed)

(defmethod mouse-event [wname :mouse-pressed] [_ s]
  (draw (r/irand 100000)))

(draw 42)
