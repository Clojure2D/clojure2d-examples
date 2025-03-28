;; https://www.youtube.com/watch?v=makaJpLvbow
;; http://zool33.uni-graz.at/artlife/PPS

;; * particle moves with constant velocity 'v'
;; * particle rotates with constant angle 'alpha'
;; * neighbourhood within radius 'r'
;; * rotate 'beta' times number of neighbours count left or right depending where the number of neighbours is higher

;; press mouse to add spores

(ns ex53-ppl
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.random :as r]            
            [clojure2d.color :as c])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const size 200)
(def ^:const scaling 4)
(def ^:const csize (* scaling size))
(def rand-size #(r/drand (* 0.2 size) (* 0.8 size)))
#_(def pal (vec (c/palette (reverse (c/palette :spectral-11)) 35)))
(def pal (vec (c/palette [:green :yellow :yellow :red :red :blue] 100)))
(def ^:const lim (dec (count pal)))

(defrecord Spore [^long id ^Vec2 pos ^double phi ^Vec2 vvec
                  ^double v ^double r ^double alpha ^double beta col])

(defn spore
  "Create spore"
  [id pos v r alpha beta col]
  (let [phi (r/drand m/TWO_PI)
        vvec (v/from-polar (v/vec2 v phi))]
    (->Spore id (or pos (v/generate-vec2 rand-size)) phi vvec v r alpha beta col)))

(defn prim+
  ([^long x] x)
  ([^long x ^long y] (+ x y)))

(defn torus
  [^Vec2 v]
  (Vec2. (m/mod (.x v) csize) (m/mod (.y v) csize)))

(defn change-move
  [{:keys [^long id pos ^double phi vvec ^double v ^double r ^double alpha ^double beta _col]} spores]
  (let [in-radius (filterv #(and (not= id (:id %)) 
                                 (< ^double (v/dist-sq pos (:pos %)) r)) spores) ;; select neighbours
        cnt (count in-radius) ;; how many neihgbours
        neighbours (mapv #(m/sgn (v/cross vvec (v/sub (:pos %) pos))) in-radius) ;; neigbours hemispheres
        ^double delta (if (pos? cnt) (* cnt beta (m/sgn (reduce prim+ 0 neighbours))) 0) ;; calc delta
        nphi (rem (+ m/TWO_PI phi alpha delta) m/TWO_PI) ;; change angle
        nvvec (v/from-polar (v/vec2 v nphi))] ;; speed vector
    (->Spore id (v/add pos nvvec) nphi nvvec v r alpha beta (pal (m/min cnt lim))))) ;; move

(defn next-spore
  "Make next spore"
  ([id] (next-spore id nil))
  ([id pos]
   ;; 180 17
   ;; a 180 b -5 !!! -10, 17 (default)
   ;; a -69 b -1
   ;; 42 9
   ;; 79.6 -1.8
   ;; 0 5
   ;; 180 179
   (spore id pos 0.67 (m/sq 11.0) (m/radians 42) (m/radians 9) (first pal))))

(defn draw
  [canvas window _ spores]
  (let [spores (if (c2d/mouse-pressed? window)
                 (conj spores (next-spore (count spores) (v/div (c2d/mouse-pos window) scaling)))
                 spores)]
    (c2d/set-background canvas (c/color 10 10 20) 100)
    (doseq [^Spore s spores
            :let [^Vec2 p (v/mult (.pos s) scaling)]]
      (c2d/set-color canvas (.col s))
      (c2d/ellipse canvas (.x p) (.y p) 8 8))
    (mapv #(change-move % spores) spores)))

(def window (c2d/show-window {:canvas (c2d/black-canvas csize csize)
                            :draw-fn draw
                            :background :black
                            :draw-state (take 100 (map next-spore (range)))}))


(comment c2d/save window "results/ex53/spores.jpg")

(m/unuse-primitive-operators)
