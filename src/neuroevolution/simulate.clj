(ns neuroevolution.simulate
  (:require [neuroevolution.chromosome :as chromosome]
            [neuroevolution.environment :as environment]
            [neuroevolution.draw :as draw]
            [neuroevolution.car]
            [fastmath.vector :as v]
            [neuroevolution.nn :as nn]
            [clojure2d.core :as c2d]
            [fastmath.stats :as stats]
            [fastmath.core :as m])
  (:import [neuroevolution.environment Environment Dot]
           [fastmath.vector Vec2]
           [neuroevolution.car Car]
           [fastmath.java Array]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; 3 4 4 2 - works for both cases

(def profile [3 4 4 2])
(def profile-eval (vec (rest profile)))

(def ^:const POPULATION-SIZE 1000)
(def ^:const ELITE-SIZE (unchecked-int (* 0.1 POPULATION-SIZE)))
(def ^:const ENVIRONMENTS 1)

(def env1 (environment/environment))
(def env2 (environment/environment))

(defn population
  [population-size profile]
  (repeatedly population-size #(chromosome/random-net profile)))

(defn step
  [^Environment env chr]
  (let [^Car car (.car env)
        ^Dot dot (.dot env)        
        ^Vec2 dir (.direction car)
        ^Vec2 to-dot (v/sub (.dot dot) (.position car))
        dist (v/mag to-dot)
        cang (m/cos (v/angle-between to-dot dir))
        in? (or (m/< dist 100.0) (m/> cang 0.99))
        ^doubles res (nn/evaluate profile-eval chr (double-array [(if in? dist -10.0) 
                                                                  cang
                                                                  (condp = (.col dot)
                                                                    :orange 10.0
                                                                    :violet -10.0
                                                                    0.0)
                                                                  1]))]
    (environment/step env (Array/aget res 0) (Array/aget res 1))))

(defn run-game
  ([chromosome] (run-game env1 #_(environment/environment) chromosome))
  ([^Environment env chr]
   (if (.game-over? env)
     (.score env)
     (recur (step env chr) chr))))

(defn run-game-multiple-envs
  [n chromosome]
  [(stats/sum (repeatedly n #(run-game chromosome))) chromosome])

(defn crossover-and-mutate
  [pop size]
  (->> (shuffle (concat pop pop (population ELITE-SIZE profile)))
       (partition 2 1)
       (mapcat chromosome/crossover)
       (map (partial chromosome/mutate 0.2))
       (take size)))

(defn evolve
  [pop]
  (let [elite (take ELITE-SIZE pop)]
    (concat elite
            (map (partial chromosome/mutate 0.4) elite)
            (crossover-and-mutate (take (m// POPULATION-SIZE 2) pop)
                                  (m/- POPULATION-SIZE (m/* 2 ELITE-SIZE))))))

(defn run-all-games
  ([] (run-all-games [0 (population POPULATION-SIZE profile)]))
  ([[^long generation pop]]
   (let [stage1 (->> (pmap (partial run-game-multiple-envs ENVIRONMENTS) pop)
                     (sort-by first >))
         scores (map first stage1)]
     (println "Generation:" generation
              "| best score:" (m// (double (first scores)) ENVIRONMENTS)
              "| average:" (m// (stats/mean scores) ENVIRONMENTS))
     [(m/inc generation) (evolve (map second stage1))])))

(defn run-all-games-n
  ([n] (run-all-games-n [0 (population POPULATION-SIZE profile)] n))
  ([pop ^long n]
   (if (m/zero? n)
     pop
     (recur (run-all-games pop) (m/dec n)))))

(def e1 (run-all-games-n 50))
(def e1 (run-all-games-n e1 50))

(defn draw-frame
  [canvas _ _ [env chr :as state]]
  (draw/draw canvas env)
  (if (:game-over? env)
    state
    [(step env chr) chr]))

(def window (c2d/show-window {:canvas (c2d/canvas 800 800 :high "Andale Mono")
                            :window-name "Car - chromosome"
                            :draw-fn draw-frame
                            :draw-state [env1 (nth (second e1) 0)]
                            :state #{}}))

#_(seq (first (second e1)))
