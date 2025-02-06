(ns neuroevolution.environment
  (:require [neuroevolution.car :as car]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.random :as r])
  (:import [fastmath.vector Vec2]
           [neuroevolution.car Car]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const TIME-LIMIT 10.0)
(def ^:const TIME-MULT 3.0)
(def ^:const TIME-STEP 0.016666666666666666)

(defrecord Dot [dot col])
(defrecord Environment [car dot dots ^double tm ^double border ^double score game-over?])

(defn make-dot [^double mid]
  (let [r (r/drand 0.1 0.8)
        a (r/drand m/TWO_PI)]
    (->Dot (-> (v/vec2 (m/cos a) (m/sin a))
               (v/mult (m/* r mid))
               (v/shift mid))
           (rand-nth [:violet :orange :green]))))

(defn environment
  ([] (environment 800.0))
  ([^double border] (environment border 10))
  ([^double border ^long dots-count]
   (let [mid (m/* border 0.5)
         car (car/car (v/vec2 mid mid))
         dots (repeatedly dots-count (partial make-dot mid))]
     (->Environment car (first dots) dots 0.0 border 0.0 false))))

(defn game-over
  ([^Environment e ^double d]
   (game-over e d (* 100.0 (count (:dots e)))))
  ([^Environment e ^double d ^double penalty]
   (let [score (m/- (.score e) penalty (m/* TIME-MULT (.tm e)) d)]
     (->Environment (.car e) (.dot e) (.dots e) (.tm e) (.border e) score (if (m/neg? penalty)
                                                                            :success :fail)))))

(defn outside?
  [^double x ^double y ^double border]
  (or (m/neg? x) (m/neg? y)
      (m/>= x border)
      (m/>= y border)))

(defn wobble [^Dot dot ^double time]
  (let [d (.dot dot)]
    (->Dot (v/add d (v/mult (v/vec2 (m/* (r/noise time) (m/cos time))
                                    (m/* (r/simplex time) (m/sin time))) 30))
           (.col dot))))

(defn in-basket? [^Car car ^Dot dot]
  (let [[orange violet] (car/baskets-position car)
        col (.col dot)
        do (v/dist (.dot dot) orange)
        dv (v/dist (.dot dot) violet)]
    (cond
      (and (= col :green) (or (m/< do 3.0) (m/< dv 3.0))) true
      (m/< do 3.0) (if (= col :orange) true :fail)
      (m/< dv 3.0) (if (= col :violet) true :fail))))

(defn step
  [^Environment e ^double acc ^double rot]
  (let [^Car car (.car e)
        ^Vec2 pos (.position car)
        ^Dot dot (.dot e)
        b? (in-basket? car dot)]
    (cond
      (.game-over? e) e
      (or (outside? (.x pos) (.y pos) (.border e))
          (m/> (.tm e) TIME-LIMIT)
          (m/invalid-double? acc)
          (m/invalid-double? rot)
          (= b? :fail)) (game-over e (v/dist pos (.dot dot)))
      b? (let [ndots (rest (.dots e))]
           (if (seq ndots)
             (->Environment (car/step car acc rot) (first ndots) ndots 0.0 (.border e)
                            (m/- (.score e) (m/* TIME-MULT (.tm e)) -100.0) false)
             (game-over e 0.0 -100.0)))
      :else (->Environment (car/step car acc rot)
                           (first (.dots e)) #_(wobble (first (.dots e)) (m/* 2 (.tm e)))
                           (.dots e) (m/+ (.tm e) TIME-STEP)
                           (.border e) (m/- (.score e) (m/* 0.2 (m/abs rot))) false))))


