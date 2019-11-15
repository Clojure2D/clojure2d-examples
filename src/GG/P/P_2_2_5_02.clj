(ns GG.P.P-2-2-5-02
  (:require [clojure2d.core :refer :all]
            [fastmath.random :as r]
            [fastmath.core :as m]
            [clojure2d.color :as c]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def wname "P_2_2_5_02")

(def ^:const ^double min-radius 3.0)
(def ^:const ^double max-radius 50.0)

(def module1-img (transcode-svg (load-svg "src/GG/data/01.svg") (* 2.0 max-radius) (* 2.0 max-radius)))
(def module2 (load-svg "src/GG/data/02.svg"))

(defn draw
  ""
  [canvas window _ lda] 
  (let [{:keys [^double mouse-rect freeze? show-line? show-svg? show-circle?]} (get-state window)
        [new-x new-y ^double new-r] (if (mouse-pressed? window)
                                      [(r/drand (- (mouse-x window) (/ mouse-rect 2.0))
                                                (+ (mouse-x window) (/ mouse-rect 2.0)))
                                       (r/drand (- (mouse-y window) (/ mouse-rect 2.0))
                                                (+ (mouse-y window) (/ mouse-rect 2.0)))
                                       1.0]
                                      [(r/drand max-radius, (- (width canvas) max-radius))
                                       (r/drand max-radius, (- (height canvas) max-radius))
                                       min-radius])
        intersect? (or freeze? (some #(let [[x y ^double r] %
                                            d (m/dist new-x new-y x y)]
                                        (< d (+ new-r r))) lda))
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
    
    (doseq [[^double x ^double y ^double r closest] new-lda]
      (let [r2 (+ r r)]
        
        (when show-svg?
          (-> canvas
              (push-matrix)
              (translate (- x (/ r2 2)) (- y (/ r2 2))))
          (if (= r max-radius)
            (image canvas module1-img 0 0)
            (image canvas (transcode-svg module2 r2 r2) 0 0))
          (pop-matrix canvas))
        
        (when show-circle?
          (-> canvas     
              (set-color :black)
              (set-stroke 1.5)
              (ellipse x y r2 r2 true)))
        
        (when show-line?
          (when-let [[cx cy] closest]
            (-> canvas
                (set-color (c/gray 150))
                (set-stroke 1.0)
                (line x y cx cy))))))

    (when (mouse-pressed? window)
      (-> canvas
          (set-color 255 200 0)
          (set-stroke 2.0)
          (crect (mouse-x window)
                 (mouse-y window)
                 mouse-rect mouse-rect true)))
    
    new-lda))

(def cnvs (canvas (* 3 374) (* 3 241)))
(def window (show-window {:window-name wname
                          :canvas cnvs
                          :draw-fn draw
                          :fps 200
                          :draw-state [[200.0 100.0 50.0 nil]]
                          :state {:mouse-rect 30.0
                                  :freeze? false
                                  :show-svg? true
                                  :show-line? false
                                  :show-circle? false}}))

(defmethod key-pressed [wname virtual-key] [e s]
  (let [^double mouse-rect (:mouse-rect s)]
    (assoc s :mouse-rect (case (key-code e)
                           :up (+ mouse-rect 4.0)
                           :down (max 4.0 (- mouse-rect 4.0))
                           mouse-rect))))

(defmethod key-pressed [wname \f] [_ s] (update-in s [:freeze?] not))
(defmethod key-pressed [wname \1] [_ s] (update-in s [:show-svg?] not))
(defmethod key-pressed [wname \2] [_ s] (update-in s [:show-line?] not))
(defmethod key-pressed [wname \3] [_ s] (update-in s [:show-circle?] not))
