(ns GG.P.P-2-2-4-02
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def wname "P_2_2_4_02")

(defn draw
  ""
  [canvas window _ lda]
  (let [r (r/drand 1 7)
        x (r/drand r (- (width canvas) r))
        y (r/drand r (- (height canvas) r))
        [_ ^double cx ^double cy ^double cr] (reduce #(let [[^double d] %1
                                                            [cx cy cr] %2
                                                            new-dist (m/dist x y cx cy)]
                                                        (if (< new-dist d)
                                                          [new-dist cx cy cr]
                                                          %1)) [1e10 0 0 0] lda)
        angle (m/atan2 (- y cy) (- x cx))
        lda (conj lda [(+ cx (* (m/cos angle) (+ cr r)))
                       (+ cy (* (m/sin angle) (+ cr r)))
                       r x y])]
    (set-background canvas :white)
    (set-stroke canvas 0.5)
    (when (get-state window)
      (doseq [[x y ^double r nx ny] (drop 1 lda)]
        (-> canvas
            (set-color 230 230 230)
            (ellipse nx ny (+ r r) (+ r r))
            (set-color :black)
            (ellipse nx ny (+ r r) (+ r r) true)
            (line nx ny x y))))

    (set-color canvas :black)
    (let [[x y ^double r] (first lda)] (ellipse canvas x y (+ r r) (+ r r) true))
    (set-color canvas 50 50 50) 
    (doseq [[x y ^double r] (drop 1 lda)] (ellipse canvas x y (+ r r) (+ r r)))
    lda))

(def cnvs (canvas 800 800 :highest))
(def window (show-window {:window-name wname
                          :canvas cnvs
                          :draw-fn draw
                          :draw-state [[(/ ^int (width cnvs) 2)
                                        (/ ^int (height cnvs) 2)
                                        360.0 0.0 0.0]]
                          :state false}))

(defmethod key-released [wname \1] [_ s] (not s))
