(ns GG.P.P-2-3-1-01
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [fastmath.core :as m]
            [clojure2d.color :as c]))

(def ^:const wname "P_2_3_1_01")

(defn draw
  ""
  [canvas window _ ^double angle]
  (let [{:keys [col ^double angle-speed ^double line-length ^double angle-off] :as state} (get-state window)]
    (when (mouse-pressed? window)
      (-> canvas
          (set-color col)
          (set-stroke 1.0)
          (translate (mouse-x window) (mouse-y window))
          (rotate (m/radians angle))
          (line 0 0 line-length 0)))
    
    (let [nangle (+ angle angle-speed angle-off)]
      
      (when-not (zero? angle-off) ;; reset trigger
        (set-state! window (assoc state :angle-off 0.0)))
      
      nangle)))

(def cnvs (canvas (* 0.95 (screen-width)) (* 0.95 (screen-height))))
(def window (show-window {:canvas cnvs
                          :window-name wname
                          :draw-fn draw
                          :state {:col (c/color 181 157 0 100)
                                  :line-length 0.0
                                  :angle-speed 1.0
                                  :angle-off 0.0}
                          :setup (fn [canvas _]
                                   (set-background canvas :white)
                                   0.0)}))

(defmethod mouse-event [wname :mouse-pressed] [_ s] (assoc s :line-length (r/drand 70.0 200.0)))

(defmethod key-released [wname \d] [_ s]
  (-> s
      (update-in [:angle-speed] #(* -1.0 ^double %))
      (update-in [:angle-off] #(+ 180.0 ^double %))))

(defmethod key-released [wname \space] [_ s] (assoc s :col (c/color (r/drand 255.0)
                                                                    (r/drand 255.0)
                                                                    (r/drand 255.0)
                                                                    (r/drand 80 150))))

(defmethod key-released [wname \1] [_ s] (assoc s :col (c/color 181 157 0 100)))
(defmethod key-released [wname \2] [_ s] (assoc s :col (c/color 0 130 164 100)))
(defmethod key-released [wname \3] [_ s] (assoc s :col (c/color 87 35 129 100)))
(defmethod key-released [wname \4] [_ s] (assoc s :col (c/color 197 0 123 100)))

(defmethod key-pressed [wname virtual-key] [e s]
  (condp = (key-code e)
    :up (update-in s [:line-length] #(+ ^double % 5.0))
    :down (update-in s [:line-length] #(- ^double % 5.0))
    :left (update-in s [:angle-speed] #(- ^double % 0.5))
    :right (update-in s [:angle-speed] #(+ ^double % 0.5))
    s))

(defmethod key-released [wname \backspace] [_ s]
  (with-canvas-> cnvs (set-background :white))
  s)

