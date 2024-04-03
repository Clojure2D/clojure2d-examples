;; https://www.youtube.com/watch?v=scvuli-zcRc
(ns ex63-particle-life
  (:require [fastmath.vector :as v]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [clojure.pprint :as pp])
  (:import [fastmath.vector Vec2]
           [fastmath.java Array]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const size 600)
(def ^:const csize 800)

(defrecord ParticleType [^long id ^double rmax color ^double friction directional?])
(defrecord Setup [^long n ^doubles alpha ^doubles beta types ^double fmult])
(defrecord Particle [^Vec2 pos ^Vec2 vel ^ParticleType ptype])

(defn random-setup
  [^long n]
  (let [n2 (m/* n n)
        alpha (double-array (repeatedly n2 #(r/randval (r/drand 0.2 1.0) (r/drand -1.0 1.0))))
        beta (double-array (repeatedly n2 #(r/drand 0.001 0.5)))
        types (mapv (fn [id c]
                      (ParticleType. id (r/drand 5.0 100.0) c (m/sqrt (r/drand 0.5 0.98))
                                     (r/brand 0.2)))
                    (range n)
                    (c/palette (c/palette) n))]
    (map->Setup {:n n :alpha alpha :beta beta :types types :fmult (r/drand 1.0 40.0)})))

(defn random-particle
  [ptype]
  (Particle. (Vec2. (r/drand 100 (+ 100 size)) (r/drand 100 (+ 100 size)))
             (v/normalize (Vec2. (r/drand -0.5 0.5) (r/drand -0.5 0.5)))
             ptype))

(defn random-particles
  [{:keys [types]} ^long cnt]
  (repeatedly cnt #(random-particle (rand-nth types))))

(defn F ^double [^double r ^double alpha ^double beta]
  (if (m/< r beta)
    (m/dec (m// r beta))
    (if (and (m/< beta r) (m/< r 1.0))
      (m/* alpha (m/- 1.0 (m// (m/abs (m/dec (m/- (m/* 2.0 r) beta)))
                               (m/- 1.0 beta))))
      0.0)))

(defn calc-force
  [^Setup setup ^Particle p1 particles]
  (let [pos (.pos p1)
        vel (.vel p1)
        
        ^ParticleType ptype (.ptype p1)
        id (.id ptype)
        rmax (.rmax ptype)
        directional? (and (.directional? ptype) (not (v/zero? vel)))
        n (.n setup)
        ^doubles alpha (.alpha setup)
        ^doubles beta (.beta setup)]
    (-> (reduce (fn [^Vec2 f ^Particle p]
                  (let [^ParticleType ptype (.ptype p)
                        r (v/sub (.pos p) pos)
                        d (v/mag r)]
                    (if (and (m/pos? d) (m/< d rmax))
                      (let [force (F (m// d rmax)
                                     (Array/get2d alpha n id (.id ptype))
                                     (Array/get2d beta n id (.id ptype)))
                            force (if directional?
                                    (m/* force (m// (m/+ 0.2 (m/abs (m/cos (v/angle-between vel r)))) 1.2))
                                    force)]
                        (v/add f (v/mult (v/div r d) force)))
                      f))) (Vec2. 0.0 0.0) particles)
        (v/mult (.fmult setup)))))

(defn fix-vel [^Vec2 p ^Vec2 v]
  (Vec2. (if (or (m/neg? (.x p))
                 (m/> (.x p) csize))
           (m/- (.x v)) (.x v))
         (if (or (m/neg? (.y p))
                 (m/> (.y p) csize))
           (m/- (.y v)) (.y v))))

(defn fix-pos [^Vec2 p]
  (Vec2. (m/constrain (.x p) 0.0 csize)
         (m/constrain (.y p) 0.0 csize)))

(defn move [setup particles ^double dt]
  (pmap (fn [^Particle p]
          (let [^ParticleType ptype (.ptype p)
                friction (.friction ptype)
                f (calc-force setup p particles)
                nv (v/add (v/mult (.vel p) friction) (v/mult f dt))
                np (v/add (.pos p) (v/mult nv dt))]
            (Particle. (fix-pos np) (fix-vel np nv) ptype))) particles))

(defn draw
  [canvas window frame [setup particles]]
  (let [particles (if (c2d/mouse-pressed? window)
                    (conj particles (Particle. (c2d/mouse-pos window)
                                               (Vec2. 0.0 0.0)
                                               (rand-nth (:types setup))))
                    particles)]
    (c2d/set-background canvas (c/color 10 10 20) 100)
    (doseq [^Particle s particles
            :let [^Vec2 p (.pos s)
                  ^ParticleType ptype (.ptype s)]]
      (c2d/set-color canvas (.color ptype))
      (c2d/ellipse canvas (.x p) (.y p) 4 4))
    #_(when (= frame 200) (c2d/save canvas "results/ex63/plife.jpg"))
    [setup (move setup particles 0.1)]))

(def window (c2d/show-window {:canvas (c2d/black-canvas csize csize :highest)
                            :draw-fn draw
                            :background :black
                            :draw-state (let [setup (random-setup (r/irand 2 6))]
                                          (pp/pprint setup)
                                          [setup (random-particles setup 1200)])}))

