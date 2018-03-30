(ns GG.P.P-2-2-4-01
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [fastmath.core :as m]))

(def wname "P_2_2_4_01")

(defn draw
  ""
  [canvas _ _ lda]
  (let [r (r/drand 1 7)
        x (r/drand r (- (width canvas) r))
        y (r/drand r (- (height canvas) r))
        [_ cx cy cr] (reduce #(let [[d] %1
                                    [cx cy cr] %2
                                    new-dist (m/dist x y cx cy)]
                                (if (< new-dist d)
                                  [new-dist cx cy cr]
                                  %1)) [1e10 0 0 0] lda)
        angle (m/atan2 (- y cy) (- x cx))
        lda (conj lda [(+ cx (* (m/cos angle) (+ cr r)))
                       (+ cy (* (m/sin angle) (+ cr r)))
                       r])]
    (set-background canvas :white)
    (set-color canvas 50 50 50) 
    (doseq [[x y r] lda] (ellipse canvas x y (+ r r) (+ r r)))
    lda))

(def cnvs (canvas (* 167 3) (* 241 3) :highest))
(def window (show-window {:window-name wname
                          :canvas cnvs
                          :draw-fn draw
                          :draw-state [[(/ (width cnvs) 2)
                                        (/ (height cnvs) 2)
                                        10]]}))
