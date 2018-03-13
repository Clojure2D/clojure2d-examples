(ns examples.ex36-spirograph
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.random :as r]
            [clojure.pprint :refer :all])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^int w 800)
(def ^:const ^int h 800)
(def ^:const ^int hw (/ w 2))
(def ^:const ^Vec2 mid (Vec2. hw hw))

(def ^:const title "Spirograph")

(def cnvs (canvas w h))

(def window (show-window cnvs title))

(defn make-spirograph
  "Create sprirograph fn based on parameters"
  ([^double k ^double l]
   (let [lk (* l k)
         rk (- 1.0 k)
         fk (/ rk k)]
     (fn [^double t]
       (let [fkt (* fk t)]
         (Vec2. (+ (* rk (m/cos t))
                   (* lk (m/cos fkt)))
                (- (* rk (m/sin t))
                   (* lk (m/sin fkt))))))))
  ([] (make-spirograph (r/drand 0.01 0.99) (r/drand 0.01 0.99))))

(defn draw-spirograph
  ""
  [canvas spirograph-fn ^long steps]
  (dotimes [x steps]
    (let [t (/ x 500.0)
          ^Vec2 res (v/add mid (v/mult (spirograph-fn t) (* 0.9 hw)))]      
      (point canvas (.x res) (.y res)))))

(with-canvas-> cnvs
  (set-background :black)
  (set-color :linen 30)
  (draw-spirograph (make-spirograph) (r/drand 10000 200000)))

(defmethod key-pressed [title \space] [_ _]
  (save cnvs (next-filename "results/ex36/" ".jpg")))
