(ns GG.P.P-2-1-1-03
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(defn draw
  "Draw lines"
  [canvas window _ _]
  (when (mouse-in-window? window)
    (let [tile-count (max 0.01 (/ (mouse-y window) 15.0))
          scl (/ (width canvas) tile-count)
          rng (r/rng :jdk (:seed (get-state window)))]
      (set-background canvas :white)
      (set-stroke canvas (max 0.1 (/ (mouse-x window) 15.0)))
      (doseq [^long grid-x (range tile-count)
              ^long grid-y (range tile-count)]
        (let [px (* grid-x scl)
              py (* grid-y scl)
              alpha-left (if (:transparent-left (get-state window))
                           (* 25.0 grid-y)
                           255)
              alpha-right (if (:transparent-right (get-state window))
                            (- 255 (* 25.0 grid-y))
                            255)]
          (if (r/brandom rng 0.5)
            (-> canvas
                (set-color (c/set-alpha (:color-left (get-state window)) alpha-left))
                (line px py (+ px (* 0.5 scl)) (+ py scl))
                (line (+ px (* 0.5 scl)) py (+ px scl) (+ py scl)))
            (-> canvas
                (set-color (c/set-alpha (:color-right (get-state window)) alpha-right))
                (line px (+ py scl) (+ px (* 0.5 scl)) py)
                (line (+ px (* 0.5 scl)) (+ py scl) (+ px scl) py))))))))

(def window (show-window {:canvas (canvas 600 600)
                          :draw-fn draw
                          :window-name "P_2_1_1_03"
                          :state {:seed 0
                                  :transparent-left false
                                  :transparent-right false
                                  :color-left (c/from-HSB* (c/color 230 255 197))
                                  :color-right (c/to-color :black)}}))

(defmethod key-pressed [(:window-name window) \1] [_ s]
  (if (= (c/from-HSB (c/color 194 187 139)) (:color-left s))
    (assoc s :color-left (c/from-HSB* (c/color 230 255 197)))
    (assoc s :color-left (c/from-HSB* (c/color 194 187 139)))))

(defmethod key-pressed [(:window-name window) \2] [_ s]
  (if (= (c/to-color :black) (:color-right s))
    (assoc s :color-right (c/from-HSB* (c/color 136 255 164)))
    (assoc s :color-right (c/to-color :black))))

(defmethod key-pressed [(:window-name window) \3] [_ s]
  (update s :transparent-left not))

(defmethod key-pressed [(:window-name window) \4] [_ s]
  (update s :transparent-right not))

(defmethod key-pressed [(:window-name window) \0] [_ s]
  (merge s {:transparent-left false
            :transparent-right false
            :color-left (c/from-HSB* (c/color 230 255 197))
            :color-right (c/to-color :black)}))

(defmethod mouse-event [(:window-name window) :mouse-pressed] [_ s] (assoc s :seed (r/irand)))
