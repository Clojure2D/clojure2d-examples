(ns GG.M.M-2-3-02
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure2d.color :as c]))

(def ^:const wname "M_2_3_02")

(defn draw
  [canvas window _ _]
  (-> canvas
      (c2d/set-background :white)
      (c2d/translate (/ (c2d/width canvas) 2)
                     (/ (c2d/height canvas) 2)))

  (let [{:keys [bold? freq-x freq-y mod-freq-x mod-freq-y phi]} (c2d/get-state window)
        point-count (+ 200 (* (max 1 (c2d/mouse-x window)) 2))
        scale-x (- (/ (c2d/width canvas) 2) 50)
        scale-y (- (/ (c2d/height canvas) 2) 50)
        max-dist (m/hypot-sqrt scale-x scale-y)]
    
    (if bold?
      
      (do
        (c2d/set-stroke canvas 8 :round)
        (loop [i (int 0)
               oldx 0.0
               oldy 0.0]
          
          (let [angle (m/norm i 0 point-count 0.0 m/TWO_PI)
                x (* scale-x (* (m/cos (* angle mod-freq-x)) (m/sin (+ (* angle freq-x) (m/radians phi)))))
                y (* scale-y (* (m/cos (* angle mod-freq-y)) (m/sin (* angle freq-y))))]

            (when (pos? i)
              (let [w (m/dist x y 0 0)
                    line-alpha (m/norm w 0 max-dist 255.0 0)]
                (-> canvas
                    (c2d/set-color (c/gray (* 2 (mod i 2))) line-alpha)
                    (c2d/line oldx oldy x y))))
            
            (when (<= i point-count) (recur (inc i) x y)))))

      (-> canvas
          (c2d/set-stroke 1)
          (c2d/set-color :black)
          (c2d/path (for [i (range (inc point-count))
                          :let [angle (m/norm i 0 point-count 0.0 m/TWO_PI)
                                x (* scale-x (* (m/cos (* angle mod-freq-x)) (m/sin (+ (* angle freq-x) (m/radians phi)))))
                                y (* scale-y (* (m/cos (* angle mod-freq-y)) (m/sin (* angle freq-y))))]]
                      (v/vec2 x y))))
      ))
  )

(def window (c2d/show-window {:canvas (c2d/canvas 600 600)
                            :window-name wname
                            :draw-fn draw
                            :state {:phi 60.0
                                    :freq-x 1.0
                                    :freq-y 4.0
                                    :mod-freq-x 2.0
                                    :mod-freq-y 1.0
                                    :bold? true}}))


(defmethod c2d/key-pressed [wname \d] [_ s] (update s :bold? not))

(defmethod c2d/key-pressed [wname \1] [_ s] (update s :freq-x #(max 1 (dec %))))
(defmethod c2d/key-pressed [wname \2] [_ s] (update s :freq-x inc))
(defmethod c2d/key-pressed [wname \3] [_ s] (update s :freq-y #(max 1 (dec %))))
(defmethod c2d/key-pressed [wname \4] [_ s] (update s :freq-y inc))

(defmethod c2d/key-pressed [wname \7] [_ s] (update s :mod-freq-x #(max 1 (dec %))))
(defmethod c2d/key-pressed [wname \8] [_ s] (update s :mod-freq-x inc))
(defmethod c2d/key-pressed [wname \9] [_ s] (update s :mod-freq-y #(max 1 (dec %))))
(defmethod c2d/key-pressed [wname \0] [_ s] (update s :mod-freq-y inc))

(defmethod c2d/key-pressed [wname c2d/virtual-key] [e s]
  (case (c2d/key-code e)
    :left (update s :phi #(+ % 15.0))
    :right (update s :phi #(- % 15.0))
    s))

