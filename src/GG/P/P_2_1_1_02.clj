(ns GG.P.P-2-1-1-02
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^double scl (/ 600.0 20.0))

(defn draw
  "Draw lines"
  [canvas window _ _]
  (when (mouse-in-window? window)
    (let [cap (:cap (get-state window))
          wx (max 0.1 (/ (mouse-x window) 10.0))
          wy (max 0.1 (/ (mouse-y window) 10.0))
          rng (r/rng :jdk (:seed (get-state window)))]
      (set-background canvas :white)
      (set-color canvas :black)
      (doseq [^long grid-x (range 20)
              ^long grid-y (range 20)]
        (let [px (* grid-x scl)
              py (* grid-y scl)]
          (if (r/brandom rng 0.5)
            (-> canvas
                (set-stroke wx cap)
                (set-color (c/set-alpha (:color-left (get-state window)) (:alpha-left (get-state window))))
                (line px py (+ px scl) (+ py scl)))
            (-> canvas
                (set-stroke wy cap)
                (set-color (c/set-alpha (:color-right (get-state window)) (:alpha-right (get-state window))))
                (line px (+ py scl) (+ px scl) py))))))))

(def window (show-window {:canvas (canvas 600 600)
                          :draw-fn draw
                          :window-name "P_2_1_1_02"
                          :state {:cap :round
                                  :seed 0
                                  :alpha-left 255
                                  :alpha-right 255
                                  :color-left (c/color 197 0 123)
                                  :color-right (c/color 87 35 129)}}))

(defmethod key-pressed [(:window-name window) \1] [_ s] (assoc s :cap :round))
(defmethod key-pressed [(:window-name window) \2] [_ s] (assoc s :cap :square))
(defmethod key-pressed [(:window-name window) \3] [_ s] (assoc s :cap :butt))

(defmethod key-pressed [(:window-name window) \4] [_ s]
  (if (= (c/to-color :black) (:color-left s))
    (assoc s :color-left (c/from-HSB* (c/color 230 255 197)))
    (assoc s :color-left (c/to-color :black))))

(defmethod key-pressed [(:window-name window) \5] [_ s]
  (if (= (c/to-color :black) (:color-right s))
    (assoc s :color-right (c/from-HSB* (c/color 194 187 130)))
    (assoc s :color-right (c/to-color :black))))

(defmethod key-pressed [(:window-name window) \6] [_ s]
  (if (= (:alpha-left s) 255)
    (assoc s :alpha-left 128)
    (assoc s :alpha-left 255)))

(defmethod key-pressed [(:window-name window) \7] [_ s]
  (if (= (:alpha-right s) 255)
    (assoc s :alpha-right 128)
    (assoc s :alpha-right 255)))

(defmethod key-pressed [(:window-name window) \0] [_ s]
  (merge s {:cap :round
            :alpha-left 255
            :alpha-right 255
            :color-left (c/to-color :black)
            :color-right (c/to-color :black)}))

(defmethod mouse-event [(:window-name window) :mouse-pressed] [_ s] (assoc s :seed (r/irand)))
