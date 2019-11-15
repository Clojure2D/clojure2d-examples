(ns GG.P.P-2-2-5-01
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def wname "P_2_2_5_01")

(def ^:const ^double min-radius 3.0)
(def ^:const ^double max-radius 50.0)

(defn draw
  ""
  [canvas window _ lda] 
  (let [{:keys [^double mouse-rect]} (get-state window)
        [new-x new-y ^double new-r] (if (mouse-pressed? window)
                                      [(r/drand (- (mouse-x window) (/ mouse-rect 2.0))
                                                (+ (mouse-x window) (/ mouse-rect 2.0)))
                                       (r/drand (- (mouse-y window) (/ mouse-rect 2.0))
                                                (+ (mouse-y window) (/ mouse-rect 2.0)))
                                       1.0]
                                      [(r/drand max-radius, (- (width canvas) max-radius))
                                       (r/drand max-radius, (- (height canvas) max-radius))
                                       min-radius])
        intersect? (some #(let [[x y ^double r] %
                                d (m/dist new-x new-y x y)]
                            (< d (+ new-r r))) lda)
        new-lda (if intersect?
                  lda
                  (let [[closest ^double new-radius] (reduce #(let [^double new-radius (second %1)
                                                                    [x y ^double r] %2
                                                                    d (m/dist new-x new-y x y)]
                                                                (if (> new-radius (- d r))
                                                                  [%2 (- d r)]
                                                                  %1))
                                                             [(first lda) (width canvas)]
                                                             lda)]
                    (conj lda [new-x new-y (min new-radius max-radius) closest])))]
    
    (set-background canvas :white)
    
    (doseq [[x y ^double r closest] new-lda]
      (-> canvas
          (set-color :black)
          (set-stroke 1.5)
          (ellipse x y (+ r r) (+ r r) true))
      
      (when-let [[cx cy] closest]
        (-> canvas
            (set-color 226 185 0)
            (set-stroke 0.75)
            (line x y cx cy))))

    (when (mouse-pressed? window)
      (-> canvas
          (set-color 255 200 0)
          (set-stroke 2.0)
          (crect (mouse-x window)
                 (mouse-y window)
                 mouse-rect mouse-rect true)))
    
    new-lda))

(def cnvs (canvas 800 800))
(def window (show-window {:window-name wname
                          :canvas cnvs
                          :draw-fn draw
                          :draw-state [[200.0 100.0 50.0 nil]]
                          :state {:mouse-rect 30.0}}))

(defmethod key-pressed [wname virtual-key] [e s]
  (let [^double mouse-rect (:mouse-rect s)]
    (assoc s :mouse-rect (case (key-code e)
                           :up (+ mouse-rect 4.0)
                           :down (max 4.0 (- mouse-rect 4.0))
                           mouse-rect))))


