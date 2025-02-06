;; https://www.youtube.com/watch?v=scvuli-zcRc

;; each particle has position/velocity/color/radius
;; each pair a->b has repel/attract linear profile

(ns ex63-particle-life
  (:require [fastmath.vector :as v]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [clojure.pprint :as pp]
            [clojure2d.extra.utils :as utils])
  (:import [fastmath.vector Vec2]
           [fastmath.java Array]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const size 600)
(def ^:const csize 800)

(defrecord ParticleType [^long id ^double rmax color ^double friction directional?])
(defrecord Setup [^long n ^doubles alpha ^doubles beta types ^double fmult])
(defrecord Particle [^Vec2 pos ^Vec2 vel ^ParticleType ptype])

(defn get-n-alpha-beta [^long n]
  [(repeatedly n #(r/randval 0.15 0.0 (r/randval (r/drand 0.2 1.0) (r/drand -1.0 1.0))))
   (repeatedly n #(r/randval 0.1 0.00001 (r/drand 0.001 0.5)))])

(defn random-setup
  [^long n]
  (let [n2 (m/* n n)
        [a b] (get-n-alpha-beta (r/randval 0.5 (r/irand 1 (m/inc n2)) n2))
        alpha (double-array (take n2 (cycle a)))
        beta (double-array (take n2 (cycle b)))
        types (mapv (fn [id c]
                      (ParticleType. id (r/drand 5.0 100.0) c (m/sqrt (r/drand 0.5 0.98))
                                     (r/brand 0.2)))
                    (range n)
                    (c/palette :category10 #_(c/palette)))]
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
    [setup (move setup particles 0.02)]))

(def setup (random-setup (r/irand 3 8)))

(def window (c2d/show-window {:canvas (c2d/black-canvas csize csize :highest)
                            :draw-fn draw
                            :background :black
                            :draw-state (do
                                          (pp/pprint setup)
                                          [setup (random-particles setup 1200)])}))



(def setups {:s4 {:n 3,
                :alpha
                [0.6684104736324896, 0.964616865419524, 0.0, 0.4797524863542435,
                 0.420585350696979, 0.0, 0.2919907666469499, 0.764963999761529,
                 0.46580437967504296],
                :beta
                [0.4789021847690479, 0.3982629457198275, 0.32274878567328485,
                 0.006632143916497076, 1.0E-5, 0.07558784022513844,
                 0.39278491978642616, 0.47501099696327787, 0.0020077077145747415],
                :types
                [{:id 0,
                  :rmax 71.43515738762513,
                  :color [31.0 119.0 180.0 255.0],
                  :friction 0.9232725105802526,
                  :directional? false}
                 {:id 1,
                  :rmax 68.68845289469147,
                  :color [255.0 127.0 14.0 255.0],
                  :friction 0.9857304980002655,
                  :directional? false}
                 {:id 2,
                  :rmax 70.19663550537409,
                  :color [44.0 160.0 44.0 255.0],
                  :friction 0.8686615546986293,
                  :directional? false}],
                :fmult 31.116278104592784}

           :s3 {:n 6,
                :alpha
                [0.694393853127492, -0.8935978076229849, 0.9183734540489523,
                 0.7517707166455021, 0.2841941531837335, 0.08925882019419396,
                 0.908367258300788, 0.23138605339016705, 0.539474804503183,
                 -0.9802196408483186, -0.8693660024733327, 0.0, 0.7318061764631272,
                 0.36360691819690205, 0.6418568962430138, -0.7204320305125755,
                 0.7380792217531813, 0.5787396658363937, -0.3291955538172251,
                 0.5226483215785241, 0.8492266307322129, 0.3096562062402062,
                 0.8063587650931772, -0.3644435757551976, 0.8051225172807546,
                 -0.5702459858224584, 0.5013477590334159, 0.6723543784338462,
                 0.7772786635514581, 0.28393901651846987, -0.6212617087079326,
                 0.7462814541882263, 0.5095448072377847, 0.6821193350277652,
                 0.744776841823634, 0.6573983356630304],
                :beta
                [0.2521608562860081, 0.4076959642424631, 0.3103972482705078,
                 0.05991445703790259, 0.33953089577942736, 0.050659182809147274,
                 0.052623099321584106, 0.254639482159116, 1.0E-5, 0.07454095722220755,
                 1.0E-5, 0.4305425458078607, 0.4371284549400973, 0.05456589836363007,
                 0.2078867381683436, 0.16536217864365724, 0.47043636960772117,
                 0.27048908630161494, 0.15059986908922987, 0.4390023446634153,
                 0.03301475339225711, 0.3720308037048845, 0.0029457710760157595,
                 0.031390696923137944, 0.1432372555387937, 0.21132657604152602,
                 0.3097446374760432, 0.031617830977375645, 0.20440732589153868,
                 0.16253467914474676, 0.3945421621765617, 1.0E-5, 0.4213522149830562,
                 0.4483283120778598, 0.1478064798692622, 0.15434745560839033],
                :types
                [{:id 0,
                  :rmax 15.771133363147245,
                  :color [31.0 119.0 180.0 255.0],
                  :friction 0.7369008895452103,
                  :directional? false}
                 {:id 1,
                  :rmax 77.88466217203033,
                  :color [255.0 127.0 14.0 255.0],
                  :friction 0.742985355025393,
                  :directional? false}
                 {:id 2,
                  :rmax 77.38995861457622,
                  :color [44.0 160.0 44.0 255.0],
                  :friction 0.7144989164158658,
                  :directional? false}
                 {:id 3,
                  :rmax 68.67660805859893,
                  :color [214.0 39.0 40.0 255.0],
                  :friction 0.8187926632606051,
                  :directional? false}
                 {:id 4,
                  :rmax 75.82489142446111,
                  :color [148.0 103.0 189.0 255.0],
                  :friction 0.9162043128890355,
                  :directional? false}
                 {:id 5,
                  :rmax 79.67260413418143,
                  :color [140.0 86.0 75.0 255.0],
                  :friction 0.8348123850263762,
                  :directional? false}],
                :fmult 30.13358133507849}
           :s2 {:n 5,
                :alpha
                [0.4096941637038132, 0.7167869425818219, -0.32250537479967933,
                 0.5386508874278341, 0.12807755622037065, 0.0, 0.4489072259997007,
                 0.9896151168560674, 0.9870059536071383, 0.9943843293539436,
                 0.7491114682697713, 0.33010843632119924, 0.7042231463915714,
                 -0.7913023790316922, 0.3694785167828138, 0.5724792191873702,
                 0.9059717885051928, 0.0, 0.0, 0.25949275669321237, 0.0,
                 0.7237415492644526, 0.0, 0.97214261378972, -0.28404642077174125],
                :beta
                [0.3311261892212369, 1.0E-5, 0.2727371400015111, 0.1715326178323534,
                 0.3053054536904885, 0.4596220521429988, 0.4506163728516102,
                 0.05611679472790398, 0.41469073616930296, 0.4662889916108893, 1.0E-5,
                 0.3534310494315174, 0.4477496639046428, 0.19391289970963096,
                 0.09647721646228848, 0.07308633354123334, 1.0E-5, 0.4458567825806172,
                 0.042402269698742015, 0.25336376918476833, 0.1604594387044829,
                 0.2552475370479657, 0.14908202819195338, 0.1715479419363129,
                 0.09240843722873682],
                :types
                [{:id 0,
                  :rmax 22.86349110354877,
                  :color [31.0 119.0 180.0 255.0],
                  :friction 0.7601831434324019,
                  :directional? false}
                 {:id 1,
                  :rmax 49.79661987038962,
                  :color [255.0 127.0 14.0 255.0],
                  :friction 0.8783872977760014,
                  :directional? true}
                 {:id 2,
                  :rmax 25.025388507912442,
                  :color [44.0 160.0 44.0 255.0],
                  :friction 0.8479055550231204,
                  :directional? false}
                 {:id 3,
                  :rmax 75.35682558801884,
                  :color [214.0 39.0 40.0 255.0],
                  :friction 0.8866316497140466,
                  :directional? true}
                 {:id 4,
                  :rmax 62.630326653746465,
                  :color [148.0 103.0 189.0 255.0],
                  :friction 0.9419449593651739,
                  :directional? false}],
                :fmult 21.256799389828544}

           :s1 {:n 3,
                :alpha
                [-0.2878465209499428, 0.3463005068872791, 0.2987004015528645,
                 0.3314686112984623, 0.9638365915599234, 0.0, 0.0, 0.409478856063085,
                 0.9782069295942872],
                :beta
                [1.0E-5, 1.0E-5, 0.11977249430498567, 0.28028181676786035,
                 0.16076709471770154, 0.1745661371553218, 0.32387773897765815,
                 0.30520192473181557, 0.130891008790613],
                :types
                [{:id 0,
                  :rmax 32.001238494811865,
                  :color [31.0 119.0 180.0 255.0],
                  :friction 0.8140996451576056,
                  :directional? false}
                 {:id 1,
                  :rmax 75.82987151812112,
                  :color [255.0 127.0 14.0 255.0],
                  :friction 0.9343131741539131,
                  :directional? false}
                 {:id 2,
                  :rmax 94.72174580527093,
                  :color [44.0 160.0 44.0 255.0],
                  :friction 0.8919310657290059,
                  :directional? false}],
                :fmult 6.227255807255642}}
  )
