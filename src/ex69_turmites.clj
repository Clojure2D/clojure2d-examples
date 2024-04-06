(ns ex69-turmites
  (:require [clojure2d.color :as c]
            [fastmath.random :as r]
            [clojure2d.pixels :as p]
            [clojure2d.core :as c2d]
            [clojure2d.extra.utils :as utils])
  (:import [fastmath.java Array]))

(def ^:const size 400)

(defrecord State [^int next-state ^int color ^int rot])

(defn random-rules
  [states colors]
  (let [rs (set (range states))
        rc (set (range colors))]
    (into {} (for [s rs
                   c rc]
               [[s c] (->State (rand-nth (vec (disj rs s)))
                               (rand-nth (vec (disj rc c)))
                               (r/irand 4))]))))

(defrecord Turmite [pos dir state])

(defn rotate [[dx dy :as d] rot]
  (case (mod (int rot) 4)
    0 d
    1 [dy (- dx)]
    2 [(- dx) (- dy)]
    3 [(- dy) dx]))

(defn add [[x y] [dx dy]] [(mod (+ x dx) size)
                        (mod (+ y dy) size)])

(defn ->arena []
  (let [colors (r/irand 2 6)
        states (r/irand 2 6)]
    {:pal (c/palette :category10)
     :rules (random-rules states colors)
     :arena (int-array (* size size))
     :pixels (p/pixels size size)
     :turmite (->Turmite [(/ size 2) (/ size 2)] [0 1] 0)}))

(defn move-and-draw
  [{:keys [pal rules ^ints arena pixels turmite] :as curr}]
  (let [{:keys [pos dir state]} turmite
        [x y] pos
        curr-color (Array/get2d arena size x y)
        {:keys [next-state color rot]} (rules [state curr-color])
        ndir (rotate dir rot)
        npos (add pos ndir)]
    (p/set-color! pixels x y (pal color))
    (Array/set2d arena size x y color)
    (assoc curr :turmite (->Turmite npos ndir next-state))))

(def ^:const speed 500)

(defn draw [canvas _ frame {:keys [pixels turmite] :as curr}]
  (let [[x y] (:pos turmite)]
    (-> canvas
        (c2d/set-background 20 20 20)
        (c2d/image (c2d/resize pixels 800 800))
        (c2d/set-color :blue 200)
        (c2d/rect 10 10 100 35)
        (c2d/set-color :white)
        (c2d/set-font-attributes 20)
        (c2d/text (str (* speed frame)) 20 35)
        (c2d/set-color :red)
        (c2d/set-stroke 2)
        (c2d/ellipse (* 2 x) (* 2 y) 10 10 true))
    (nth (iterate move-and-draw curr) speed)))

(def window (c2d/show-window {:canvas (c2d/canvas 800 800)
                            :background :black
                            :draw-fn draw
                            :draw-state (->arena)}))
