(ns GG.M.M-1-3-03
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [clojure2d.pixels :as p]
            [fastmath.core :as m]
            [clojure2d.color :as c]))

(def ^:const wname "M_1_3_03")

(defn draw
  "Draw noise"
  [canvas window _ state]

  (let [noise (r/fbm-noise (:noise-cfg (get-state window)))
        [nx-range ny-range] (if (or (neg? (mouse-x window))
                                    (neg? (mouse-y window)))
                              state
                              [(/ (mouse-x window) 10.0)
                               (/ (mouse-y window) 10.0)])
        first-mode? (= 1 (:noise-mode (get-state window)))]

    (p/set-canvas-pixels! canvas (p/filter-colors-xy (fn [_ x y]
                                                       (let [noise-x (m/norm x 0 (width canvas) 0 nx-range)
                                                             noise-y (m/norm y 0 (height canvas) 0 ny-range)
                                                             noise-value (if first-mode?
                                                                           (* 255.0 ^double (noise noise-x noise-y))
                                                                           (* 255.0 (m/frac (* 24.0 ^double (noise noise-x noise-y)))))]
                                                         (c/gray noise-value))) (p/to-pixels canvas)))

    
    [nx-range ny-range]))

(def window (show-window {:canvas (canvas 512 512)
                          :window-name wname
                          :state {:noise-cfg (r/random-noise-cfg)
                                  :noise-mode 1}
                          :draw-state [300 300]
                          :draw-fn draw}))

(defmethod key-pressed [wname \space] [_ s]
  (let [ns (assoc s :noise-cfg (r/random-noise-cfg))]
    (println ns)
    ns))

(defmethod key-pressed [wname \1] [_ s] (assoc s :noise-mode 1))
(defmethod key-pressed [wname \2] [_ s] (assoc s :noise-mode 2))

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
    (println ns)
    ns))
