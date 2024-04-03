;; https://www.karlsims.com/rd.html

;; press space to select k and f
;; press m to back to a map
;; left click to seed B

(ns ex65-reaction-diffusion
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure2d.core :as c2d]
            [clojure2d.color :as c]
            [fastmath.vector :as v])
  (:import [fastmath.java Array]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const size 450)
(def ^:const scale 2)
(def ^:const csize (m/* scale size))
(def ^:const size2 (m/* size size))
(def rsize (range size))

(defn setup []
  (let [A (double-array size2 1.0)
        dA (double-array size2)

        B (double-array size2 0.0)
        dB (double-array size2)]

    (doseq [^long x (range 50 100)
            ^long y (range 120 180)]
      (Array/set2d B size x y (r/drand))
      (Array/set2d B size y x (r/drand)))

    (doseq [^long x (repeatedly 100 #(r/irand size))
            ^long y (repeatedly 100 #(r/irand size))]
      (Array/set2d B size x y (r/drand))
      (Array/set2d B size y x (r/drand)))

    {:A A :dA dA
     :B B :dB dB
     :Da 1.0 :Db m/THIRD
     :f 0.055 :k 0.062
     :dt 1.0
     :show-map? true}))

(defn diffuse! [^double d ^doubles source ^doubles target]
  (doseq [^long x rsize ^long y rsize
          :let [x- (m/mod (m/dec x) size)
                x+ (m/mod (m/inc x) size)
                y- (m/mod (m/dec y) size)
                y+ (m/mod (m/inc y) size)]]
    (Array/set2d target size x y (m/* d (m/- (m/+ (m/* 0.05 (m/+ (Array/get2d source size x- y-)
                                                                 (Array/get2d source size x+ y-)
                                                                 (Array/get2d source size x- y+)
                                                                 (Array/get2d source size x+ y+)))
                                                  (m/* 0.2 (m/+ (Array/get2d source size x- y)
                                                                (Array/get2d source size x+ y)
                                                                (Array/get2d source size x y-)
                                                                (Array/get2d source size x y+))))
                                             (Array/get2d source size x y))))))

(defn norm-f ^double [^double y] (m/mnorm y 0 size 0.15 0.001))
(defn norm-k ^double [^double x] (m/mnorm x 0 size 0.041 0.085))

(defn step [{:keys [^doubles A ^doubles dA ^doubles B ^doubles dB ^double dt
                 ^double Da ^double Db ^double f ^double k show-map?] :as data}]
  (diffuse! Da A dA)
  (diffuse! Db B dB)
  (doseq [^long x rsize ^long y rsize
          :let [a (Array/get2d A size x y)
                b (Array/get2d B size x y)
                abb (m/* a b b)
                f (if show-map? (norm-f y) f)
                k (if show-map? (norm-k x) k)]]
    (Array/set2d A size x y (m/constrain (m/+ a (m/* dt (m/+ (Array/get2d dA size x y)
                                                             (m/- (m/* f (m/- 1.0 a)) abb)))) 0.0 1.0))
    (Array/set2d B size x y (m/constrain (m/+ b (m/* dt (m/+ (Array/get2d dB size x y)
                                                             (m/- abb (m/* (m/+ f k) b))))) 0.0 1.0)))
  data)

(defn draw
  [canvas window _ {:keys [^doubles A ^doubles B show-map? f k] :as curr}]
  (when (c2d/mouse-pressed? window)
    (let [[x y] (v/div (c2d/mouse-pos window) scale)]
      (dotimes [_ 20] (Array/set2d B size
                                   (m/mod (m/+ (r/irand -10 10) (int x)) size)
                                   (m/mod (m/+ (r/irand -10 10) (int y)) size) (r/drand)))))
  (let [[^double mx ^double my] (v/div (c2d/mouse-pos window) scale)
        curr (if (c2d/key-pressed? window)
               (condp = (c2d/key-code window)
                 :space  (let []
                           (assoc curr
                                  :f (norm-f my)
                                  :k (norm-k mx)
                                  :show-map? false))
                 :m (assoc curr :show-map? true)
                 curr)
               curr)]
    
    (c2d/set-background canvas (c/color 10 10 20) 100)
    (doseq [^long x rsize ^long y rsize
            :let [a (Array/get2d A size x y)
                  b (Array/get2d B size x y)]]
      (c2d/set-color canvas (v/mult (v/vec3 (m/- 1.0 a) b (m/sqrt (m/* a b))) 255.0))
      (c2d/rect canvas (m/* x scale) (m/* y scale) scale scale))
    (let [f (m/approx (if show-map? (norm-f my) f) 6)
          k (m/approx (if show-map? (norm-k mx) k) 6)]
      (-> canvas
          (c2d/set-color :light-blue 200)
          (c2d/rect 8 9 80 38)
          (c2d/set-color :black)
          (c2d/set-font-attributes 12 :bold)
          (c2d/text (str "f=" f) 15 25)
          (c2d/text (str "k=" k) 15 40)))
    (step curr)))

(def window (c2d/show-window {:canvas (c2d/black-canvas csize csize :mid)
                            :draw-fn draw
                            :background :black
                            :draw-state (step (setup))}))

(comment (c2d/save window "results/ex65/rd.jpg"))
