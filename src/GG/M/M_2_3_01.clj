(ns GG.M.M-2-3-01
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.vector :as v]))

(def ^:const wname "M_2_3_01")

(defn draw-shapes
  ""
  [canvas {:keys [phi freq mod-freq draw-frequency? draw-modulation?]}]
  (let [scaling (* (/ (height canvas) 4))
        info-fn #(m/sin (+ (* % freq) (m/radians phi)))
        carrier-fn #(m/cos (+ (* % mod-freq)))
        angles (map #(vector % (m/norm % 0 (width canvas) 0 m/TWO_PI)) (range (width canvas)))]
    
    (-> canvas 
        (set-background :white)
        (translate 0 (* scaling 2)))

    (when draw-frequency?
      (-> canvas
          (set-color 0 130 164)
          (path (for [[i angle] angles]
                  (v/vec2 i (* scaling (info-fn angle)))))))

    (when draw-modulation?
      (-> canvas
          (set-color 0 130 164 128)
          (path (for [[i angle] angles]
                  (v/vec2 i (* scaling (carrier-fn angle)))))))

    (-> canvas
        (set-color :black)
        (set-stroke 2.0)
        (path (for [[i angle] angles
                    :let [info (info-fn angle)
                          carrier (carrier-fn angle)]]
                (v/vec2 i (* info carrier scaling)))))))

(def cnvs (canvas 800 400))
(def window (show-window {:canvas cnvs
                          :window-name wname
                          :state {:phi 0.0
                                  :freq 2.0
                                  :mod-freq 12.0
                                  :draw-frequency? true
                                  :draw-modulation? true}}))

(defn draw
  ""
  [s]
  (with-canvas-> cnvs (draw-shapes s))
  s)


(defmethod key-pressed [wname \i] [_ s] (draw (update s :draw-frequency? not)))
(defmethod key-pressed [wname \c] [_ s] (draw (update s :draw-modulation? not)))

(defmethod key-pressed [wname \1] [_ s] (draw (update s :freq #(max 1 (dec %)))))
(defmethod key-pressed [wname \2] [_ s] (draw (update s :freq inc)))

(defmethod key-pressed [wname \7] [_ s] (draw (update s :mod-freq #(max 1 (dec %)))))
(defmethod key-pressed [wname \8] [_ s] (draw (update s :mod-freq inc)))

(defmethod key-pressed [wname virtual-key] [e s]
  (case (key-code e)
    :left (draw (update s :phi #(+ % 15.0)))
    :right (draw (update s :phi #(- % 15.0)))
    s))

(draw (get-state window))
