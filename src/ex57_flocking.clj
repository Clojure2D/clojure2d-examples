;; This is the limited port of paperjs example http://paperjs.org/examples/tadpoles/
;; it is not implementing movement along a path.

(ns examples.ex57-flocking
  (:require [clojure2d.core :refer :all]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v])
    (:import [fastmath.vector Vec2]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(m/use-primitive-operators)

(def ^:const ^double w 1000)
(def ^:const ^double h 600)

(def ^Vec2 zero-vec (Vec2. 0 0))

(defn mk-boid [^Vec2 position ^double max-speed ^double max-force]
  (let [strength (r/drand 0 0.5)
        amount (+ (* strength 10) 10)]
    {:acceleration (Vec2. 0 0)
     :vector (Vec2. (r/drand -2 2) (r/drand -2 2))
     :position position
     :radius 30
     :max-speed (+ max-speed strength)
     :max-force (+ max-force strength)
     :amount amount
     :count 0
     :head {:size [13 8]}
     :path (mapv (fn [_] zero-vec) (range amount) )
     :short-path (mapv (fn [_] zero-vec) (range (m/min 3 amount)))
     }))

(defn steer [this ^Vec2 target slowdown]
  (let [desired (v/sub target (:position this))
        distance (v/mag desired)
        dl (if (and slowdown (< distance 100))
             (* ^double (:max-speed this) (/ distance 100))
             (:max-speed this))
        steer-v (v/sub (v/set-mag desired dl) (:vector this))]
    (v/limit steer-v ^double (:max-force this))))



(defn seek [{ acc :acceleration :as this} ^Vec2 target]
  (update this :acceleration (partial v/add (steer this target false))))


(defn arrive [{ acc :acceleration :as this} ^Vec2 target]
  (update this :acceleration (partial v/add (steer this target true))))


(defn align [this boids]
  (let [nd 35.0
        [s ^double c] (reduce (fn [[^Vec2 ste ^double cnt] b]
                                (let [dst (v/dist (:position this) (:position b))]
                                  (if (and (pos? dst) (< dst nd) )
                                    [(v/add ste (:vector b)) (inc cnt)]
                                    [ste cnt]))) [zero-vec 0] boids)
        s' (if (pos? c) (v/div s c) s)]
    (if (not= 0 (v/mag s'))

      (let [sl (v/set-mag s' (:max-speed this))
            sv (v/sub sl  (:vector this))]
        (v/limit sv (:max-force this)))
      
      s')))
        
(defn cohesion [this boids]
  (let [nd 120
        [^Vec2 s ^double c] (reduce (fn [[ste ^double cnt] b]
                                      (let [dst (v/dist (:position this) (:position b))]
                                        (if (and (pos? dst) (< dst nd) )
                                          [(v/add ste (:position b)) (inc cnt)]
                                          [ste cnt]))) [zero-vec 0] boids)]
    (if (pos? c)
      (steer this (v/div s c) false)
      s)))


(defn separate [this boids]
  (let [des-sep 80
        [s ^double c] (reduce (fn [[^Vec2 ste ^double cnt] b]
                                (let [vect (v/sub (:position this) (:position b))
                                      dst (v/mag vect)]
                                  (if (and (pos? dst) (< dst des-sep))
                                    [(v/add ste (v/mult (v/normalize vect) (/ 1.0 dst))) (inc cnt)]
                                    [ste cnt]))) [zero-vec 0] boids)
        s' (if (pos? c) (v/div s c) s)]
    (if (not= 0 (v/mag s'))
      (let [sl (v/set-mag s' (:max-speed this))
            sv (v/sub sl (:vector this))]
        (v/limit sv ^double (:max-force this)))
      
      s')))


(defn flock [this boids]
  (let [s (v/mult (separate this boids) 0.6)
        a (align this boids)
        c (cohesion this boids)]
    (assoc this :acceleration (v/add (:acceleration this) (v/add  s (v/add a c))))) ) 


(defn update-boid [{:keys [vector position acceleration max-speed] :as b}]
  (let [speed (v/add vector acceleration)
        vec (v/limit speed max-speed)]
    (assoc b :vector vec :position (v/add position vec) :acceleration zero-vec)))
      
(defn draw-head [cvs {:keys [head]  :as b}]
  (let [ang (v/heading (:vector b))
        [x y] (:position b)
        [ew eh] (:size head)]
    (with-canvas-> cvs
      (push-matrix)
      (translate x y)
      (rotate ang)
      (ellipse 0 0 ew eh)
      (pop-matrix)))
  b)


(defn initial-state []
  {:boids (repeatedly 30 #(mk-boid (Vec2. (r/drand w) (r/drand h)) 10 0.05))
   :group false})

(defn borders [{:keys [position ^double radius] :as boid}]
  (let [[^double px ^double py] position
        vv
        (->> [0 0]
             ((fn [[x y]] [(if (neg? (+ px radius)) (+ w radius) x) y]))
             ((fn [[x y]] [x (if (neg? (+ py radius)) (+ h radius) y)]))
             ((fn [[x y]] [(if (> px (+ w radius)) (+ (- w) (- radius)) x) y]))
             ((fn [[x y]] [x (if (> py (+ h radius)) (+ (- h) (- radius)) y)]))
             (apply v/vec2 ))]
    (if (not= (v/mag vv) 0)
      (assoc boid :position (v/add position vv) :path (mapv #(v/add vv %) (:path boid))) 
      boid)))
      


(defn calc-tail [cvs this]
  (let [speed (v/mag (:vector this))
        pl (+ 5 (/ speed 3.0)) 

        [seg ss c] (loop [point (:position this)
                          last-vec (v/mult (:vector this) -1)
                          seg (assoc (:path this) 0 point)
                          s-seg (assoc (:short-path this) 0 point)
                          ^double cnt (:count this)
                          i 1]
                     (if (< i ^double (:amount this))
                       (let [vect (v/sub (nth seg i) point)
                             c (+ cnt (* speed 10))
                             wave (m/sin (/ (+ c (* i 3)) 300))
                             sway  (v/mult (v/normalize (v/rotate last-vec  m/HALF_PI)) wave)
                             p (v/add point (v/add (v/mult (v/normalize last-vec) pl) sway))]
                         (recur p vect (assoc seg i p) (if (< i 3) (assoc s-seg i p) s-seg) c (inc i)))
                       [seg s-seg cnt]))]
    (set-stroke cvs 4)
    (path cvs ss)
    (set-stroke cvs 2)
    (path cvs seg)
    (assoc this :path seg :short-path ss :count c)
    ) )



(defn run-boids [canvas boid {:keys [group boids] :as state}]
  (let [b (assoc boid :last-loc (:position boid))]

    (->> b
         ((fn [b] (if group
                    b
                    (flock b boids))))
         (borders)
         (update-boid)
         (calc-tail canvas)
         (draw-head canvas ))))


(defn get-path-target [^long i ^long n ^long f]
  (let [f' (long (/ f 30))
        a (* m/TWO_PI (/ (double(mod (+ i f') n))  (double n))) ]
    (v/add (v/vec2 (/ w 2) (/ h 2)   )    (v/mult (v/vec2 (m/cos a ) (m/sin a)) (* h 0.4)))))




(let [canvas (canvas w h :high)
      draw (fn [cvs wnd frm  state]
             (let [ev (get-state wnd)
                   gr (:group state)
                   state (assoc state :group (if (= ev :change) (not gr) gr))
                   {:keys [boids group]} state
                   cb (count boids)]
               (set-state! wnd :none)
               (set-background cvs :black)
               (set-color cvs :white)
               (text cvs "click in wndow for a surprise" 10 16)
               
               (assoc state :boids (vec (map-indexed
                                         (fn [i b]
                                           (let [b' (if group (arrive b (get-path-target i cb frm)) b)]
                                             
                                             (run-boids cvs b' state))) boids)))))
      
      wnd (show-window {:canvas canvas
                        :draw-fn draw
                        :window-name "boids"
                        :draw-state (initial-state)})]
  (defmethod mouse-event ["boids" :mouse-pressed] [e _]
    (set-state! wnd :change)))
      
