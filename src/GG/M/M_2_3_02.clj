(ns GG.M.M-2-3-02
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [clojure2d.color :as c]))

(def ^:const wname "M_2_3_02")

(defn draw
  ""
  [canvas window _ _]
  (-> canvas
      (set-background :white)
      (translate (/ (width canvas) 2)
                 (/ (height canvas) 2)))

  (let [{:keys [bold? freq-x freq-y mod-freq-x mod-freq-y phi]} (get-state window)
        point-count (+ 200 (* (max 1 (mouse-x window)) 2))
        scale-x (- (/ (width canvas) 2) 50)
        scale-y (- (/ (height canvas) 2) 50)
        max-dist (m/hypot-sqrt scale-x scale-y)]
    
    (if bold?
      
      (do
        (set-stroke canvas 8 :round)
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
                    (set-color (c/gray (* 2 (mod i 2))) line-alpha)
                    (line oldx oldy x y))))
            
            (when (<= i point-count) (recur (inc i) x y)))))

      (-> canvas
          (set-stroke 1)
          (set-color :black)
          (path (for [i (range (inc point-count))
                      :let [angle (m/norm i 0 point-count 0.0 m/TWO_PI)
                            x (* scale-x (* (m/cos (* angle mod-freq-x)) (m/sin (+ (* angle freq-x) (m/radians phi)))))
                            y (* scale-y (* (m/cos (* angle mod-freq-y)) (m/sin (* angle freq-y))))]]
                  (v/vec2 x y))))
      ))
  )

(def window (show-window {:canvas (canvas 600 600)
                          :window-name wname
                          :draw-fn draw
                          :state {:phi 60.0
                                  :freq-x 1.0
                                  :freq-y 4.0
                                  :mod-freq-x 2.0
                                  :mod-freq-y 1.0
                                  :bold? true}}))


(defmethod key-pressed [wname \d] [_ s] (update s :bold? not))

(defmethod key-pressed [wname \1] [_ s] (update s :freq-x #(max 1 (dec %))))
(defmethod key-pressed [wname \2] [_ s] (update s :freq-x inc))
(defmethod key-pressed [wname \3] [_ s] (update s :freq-y #(max 1 (dec %))))
(defmethod key-pressed [wname \4] [_ s] (update s :freq-y inc))

(defmethod key-pressed [wname \7] [_ s] (update s :mod-freq-x #(max 1 (dec %))))
(defmethod key-pressed [wname \8] [_ s] (update s :mod-freq-x inc))
(defmethod key-pressed [wname \9] [_ s] (update s :mod-freq-y #(max 1 (dec %))))
(defmethod key-pressed [wname \0] [_ s] (update s :mod-freq-y inc))

(defmethod key-pressed [wname virtual-key] [e s]
  (case (key-code e)
    :left (update s :phi #(+ % 15.0))
    :right (update s :phi #(- % 15.0))
    s))

