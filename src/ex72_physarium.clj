;; https://softologyblog.wordpress.com/2019/04/11/physarum-simulations/
(ns ex72-physarium
  (:require [fastmath.vector :as v]
            [fastmath.core :as m]
            [clojure2d.pixels :as p]
            [fastmath.random :as r]
            [clojure2d.core :as c2d]
            [clojure2d.color :as c]
            [fastmath.fields :as f])
  (:import [fastmath.vector Vec2]
           [clojure2d.pixels GradientRenderer]
           [clojure2d.java GradientDensity]
           [clojure2d.java.filter Blur]
           [fastmath.java Array]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const SIZE 800)
(def ^:const MID (m// SIZE 2))
(def ^:const Q1 (m/- MID 200))
(def ^:const Q2 (m/+ MID 200))

(defrecord Particle [^Vec2 pos ^Vec2 dir])

(defrecord ParticleConfig [^double damount ;; deposit amount
                           ^double speed ;; speed
                           ^double rangle ;; rotation angle
                           ^double sangle ;; sensor angle
                           ^double slen ;; sensor length
                           ^double rlen ;; real length
                           ^double neg]) ;; negative angle

(defn particle []
  (let [a (r/drand m/TWO_PI)]
    (Particle. #_(Vec2. (r/drand Q1 Q2) (r/drand Q1 Q2))
               (v/shift (v/mult (Vec2. (m/cos a) (m/sin a)) (m/+ 200 (m/* 10 (r/irand 10)))) MID)
               (let [a (r/drand m/TWO_PI)] (Vec2. (m/cos a) (m/sin a))))))

(def default-config {:damount 5.0 :speed 1.0 :rangle (m/radians 30) :sangle (m/radians 30) :slen 1.0 :rlen 1.0
                   :neg 1.0})

(defn config
  ([] (config {}))
  ([m] (map->ParticleConfig (merge default-config m))))

(defn diffuse
  [^GradientRenderer arena ^long blr]
  (Blur/boxBlur (.cnt ^GradientDensity (.buff arena))
                (.cnt ^GradientDensity (.buff arena)) SIZE SIZE blr)
  arena)

(defn decay
  [^GradientRenderer arena ^double amount]
  (let [^doubles b (.cnt ^GradientDensity (.buff arena))]
    (dotimes [i (alength b)]
      (Array/aset b i (m/constrain (m/* amount (Array/aget b i)) 0.0 2.0))))
  arena)

(binding [f/*skip-random-fields* true]
  (def f1 (f/combine (f/random-configuration 2))))

(defn sense
  ^double [arena ^ParticleConfig pc ^Particle p]
  (let [#_#_fpos (v/mult (v/normalize (v/shift (.pos p) (m/- MID))) 2.0)
        #_#_fv (f1 fpos)
        dir (v/mult (.dir p) (m/* #_(m/sin (v/magsq fv)) (.slen pc)))
        #_#_^Vec2 vpos (v/round (v/add (.pos p) (v/mult (.dir p) (.rlen pc))))
        #_#_v (double (p/get-pixel arena (m/mod (.x vpos) SIZE) (m/mod (.y vpos) SIZE)))
        sangle (m/+ (.sangle pc) #_(m/* 1 (m/pow v 2)))
        rangle (m/+ (.rangle pc) #_(m/* 0 (m/pow v 0.4)))
        ^Vec2 left (v/round (v/add (.pos p) (v/rotate dir (m/- sangle))))
        ^Vec2 right (v/round (v/add (.pos p) (v/rotate dir sangle)))
        ^Vec2 ahead (v/round (v/add (.pos p) dir))
        lamt (double (p/get-pixel arena (m/mod (.x left)  SIZE) (m/mod (.y left)  SIZE)))
        ramt (double (p/get-pixel arena (m/mod (.x right) SIZE) (m/mod (.y right) SIZE)))
        aamt (double (p/get-pixel arena (m/mod (.x ahead) SIZE) (m/mod (.y ahead) SIZE)))]
    (cond
      (and (m/> aamt lamt) (m/> aamt ramt)) 0.0
      (and (m/< aamt lamt) (m/< aamt ramt)) (r/randval (m/- rangle) rangle)
      (m/< lamt ramt) rangle
      (m/> lamt ramt) (m/- rangle)
      :else (r/randval (m/- rangle) rangle))))

(defn rotate-and-move
  [^ParticleConfig pc ^Particle p ^double angle]
  (let [ndir (if (m/zero? angle)
               (.dir p)
               (v/normalize (v/rotate (.dir p) (m/* (.neg pc) angle))))
        vel (v/mult ndir (.speed pc))
        npos (v/add (.pos p) vel)]
    (Particle. npos ndir)))

(defn in [^Particle p]
  (let [^Vec2 pos (.pos p)]
    (Particle. (Vec2. (m/mod (.x pos) SIZE)
                      (m/mod (.y pos) SIZE))
               (.dir p))))

(defn sense-rotate-and-move
  [arena pc p]
  (->> (sense arena pc p)
       (rotate-and-move pc p)
       (in)))

(defn deposit
  [arena ^ParticleConfig pc ^Particle p]
  (let [^Vec2 pos (.pos p)]
    (p/add-pixel! arena (.x pos) (.y pos) (.damount pc))))

(defn step-particles
  [arena pc dcy blr particles]
  (let [nparticles (map (partial sense-rotate-and-move arena pc) particles)]
    (run! (partial deposit arena pc) nparticles)
    (diffuse arena blr)
    (decay arena dcy)
    nparticles))

(def c1 {:damount 5.0 :speed 2.8447638 :rangle 0.97047323 :sangle 0.87946403 :slen 42.838207 :rlen 100.0})
(def c2 {:damount 5.0, :speed -2.123094486401656, :rangle 1.1431728747139345, :sangle 2.1916797552460983, :slen 18.280961853302735, :neg -1.0})
(def c3 {:damount 5.0, :speed 1.5085066787638617, :rangle 1.4271251950874746, :sangle 0.36228793450958185, :slen 3.8233180785787746, :neg 1.0})
(def c4 {:damount 5.0, :speed 0.8678310795792452, :rangle 5.718435084606038, :sangle 0.2883419706326977, :slen 27.0913007774587, :neg -1.0})
(def c5 {:damount 5.0, :speed 2.8336336509119526, :rangle 4.796695504822241, :sangle 5.4930411685050755, :slen 8.11220451698973, :neg 1.0})
(def c6 {:damount 5.0, :speed -2.638722084981284, :rangle 1.7451995447297763, :sangle 5.436618465808182, :slen 4.974584993766939, :rlen 7.134218933754822, :neg 1.0})
(def c7 {:damount 5.0, :speed 2.674274989604731, :rangle 1.9160646244818422, :sangle 2.0013430480247165, :slen 10.793536289064336, :rlen 35.879466937033435, :neg 1.0})
(def c8 {:damount 5.0, :speed 2.674274989604731, :rangle 1.9160646244818422, :sangle 2.0013430480247165, :slen 10.793536289064336, :rlen 35.879466937033435, :neg 1.0})

(def rc {:damount 5.0
       :speed (r/drand -3 3)
       :rangle (r/drand m/TWO_PI)
       :sangle (r/drand m/TWO_PI)
       :slen (r/drand 50.0)
       :rlen (r/drand 50.0)
       :neg (r/randval 0.2 -1.0 1.0)})

(def rc2 (let [a (r/drand m/TWO_PI)]
         {:damount 5.0
          :speed (r/drand -3 3)
          :rangle (m/radians 90)
          :sangle (m/radians 10)
          :slen (r/drand 50.0)
          :neg (r/randval 0.2 -1.0 1.0)}))

(println rc)

(def gradient (c/gradient [:black :docc/deep-lyons-blue :lightblue (c/gray 200) :white]))

(defn draw
  [canvas _ _ [arena cfg dcy blr particles]]
  (let [nparticles (step-particles arena cfg dcy blr particles)
        #_#_ncfg (-> (update cfg :sangle + 0.001)
                     (update :rangle - 0.001234))]
    (c2d/image canvas (p/to-pixels arena {:gradient gradient}))
    [arena cfg dcy blr nparticles]))

(def window (c2d/show-window {:canvas (c2d/canvas 800 800)
                            :draw-state [(p/gradient-renderer SIZE SIZE)
                                         (config c3) 0.1 1
                                         (repeatedly 100000 particle)]
                            :draw-fn draw}))


