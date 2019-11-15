(ns GG.M.M-1-5-01
  (:require [clojure2d.core :refer :all]
            [clojure2d.color :as c]
            [fastmath.random :as r]
            [fastmath.core :as m]
            [fastmath.fields :as f]))

(def ^:const wname "M_1_5_01")

(def ^:const w 800)
(def ^:const h 800)
(def ^:const arc-color (c/color 0 130 164 100))
(def ^:const tile-size 40.0)
(def ^:const tile-size-75 (* 0.75 tile-size))
(def ^:const tile-size-25 (* 0.25 tile-size))
(def ^:const grid-resolution-x (m/round (/ w tile-size)))
(def ^:const grid-resolution-y (m/round (/ h tile-size)))

(def arrow (transcode-svg (load-svg "src/GG/data/arrow.svg") tile-size-75 tile-size-75))

(defn draw
  ""
  [canvas window _ _]
  (let [{:keys [noise debug]} (get-state window)
        noise-x-range (/ (max 1 (mouse-x window)) 100.0)
        noise-y-range (/ (max 1 (mouse-y window)) 100.0)]
    (set-background canvas :white)

    (dotimes [gy (inc grid-resolution-y)]
      (dotimes [gx (inc grid-resolution-x)]
        (let [noise-x (m/norm gx 0 grid-resolution-x 0 noise-x-range)
              noise-y (m/norm gy 0 grid-resolution-y 0 noise-y-range)
              ^double noise-value (noise noise-x noise-y)
              angle (* noise-value m/TWO_PI)]

          (-> canvas
              (push-matrix)
              (translate (* tile-size gx) (* tile-size gy)))

          (when debug
            (-> canvas
                (set-color (c/gray (* noise-value 255.0)))
                (ellipse 0 0 tile-size-25 tile-size-25)))

          (-> canvas

              (set-stroke 1.0 :square)
              (set-color arc-color)
              (arc 0 0 tile-size-75 tile-size-75 0 angle)
              
              (rotate angle)
              (image arrow 0 0)
              (pop-matrix)))))))


(def window (show-window {:canvas (canvas w h)
                          :window-name wname
                          :draw-fn draw
                          :state (let [nc (r/random-noise-cfg)]
                                   {:noise-cfg nc
                                    :noise (r/fbm-noise nc)
                                    :debug true})}))


(defmethod key-pressed [wname \space] [_ s]
  (let [nc (r/random-noise-cfg)
        ns (assoc s :noise-cfg nc :noise (r/fbm-noise nc))]
    (println ns)
    ns))

(defmethod key-pressed [wname \d] [_ s] (update s :debug not))

(defmethod key-pressed [wname virtual-key] [e s]
  (let [^double falloff (get-in s [:noise-cfg :gain])
        ^long octaves (get-in s [:noise-cfg :octaves])
        ^double lacunarity (get-in s [:noise-cfg :lacunarity])
        ns (condp = (key-code e)
             :up (assoc-in s [:noise-cfg :gain] (m/constrain (+ falloff 0.05) 0.0 1.0))
             :down (assoc-in s [:noise-cfg :gain] (m/constrain (- falloff 0.05) 0.0 1.0))
             :left (assoc-in s [:noise-cfg :octaves] (max 1 (dec octaves)))
             :right (assoc-in s [:noise-cfg :octaves] (inc octaves))
             :page_up (assoc-in s [:noise-cfg :lacunarity] (+ lacunarity 0.1))
             :page_down (assoc-in s [:noise-cfg :lacunarity] (- lacunarity 0.1))
             s)]
    (println (:noise-cfg ns))
    (assoc ns :noise (r/fbm-noise (:noise-cfg ns)))))
