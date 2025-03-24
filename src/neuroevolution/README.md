# Neuroevolution algorithm

Train simple neural networks using genetic algorithms to play a game.

`(require '[neuroevolution.core :refer :all]
          '[neuroevolution.simulate :as sim]
		  '[neuroevolution.environment :as env])`

## Game

Call `(play-game)` to run a game and play it. Keys:

- `j` - left
- `l` - right
- `i` - forward
- `k` - backward

The goal is to collect dots into baskets. You have to match color unless dot is green.

Fail when:
- you go outside the window
- time to collect a dot is over
- wrong basket used to collect a dots

Points are given (bonus) for:
- dot collected: 100
- finish: 200 

Points are taken out (penalty) for:
- fail, 100 * dots left
- time (per dot), time-mult * time left
- rotation, rot-mult * rotation

## Simulation

### Neural network

Input:

- distance to a dot, only when in radar range, `-10.0` otherwise
- cosine of angle between direction and a dot
- color of the dot:
  - orange: `10.0`
  - violet: `-10.0`
  - green: `0.0`

Output from [-1.0, 1.0]:
- forward/backward acceleration
- left/right rotation speed

Activation function used: softsign, f(x)=x/(1+|x|)

### Evolution

Genome: weights and biases

Every generation:

- worst half of the population is dropped
- elite is selected (`:elite-size`)
- copy of elite is highly mutated (up to 40% per genome)
- the rest is created from crossover and mutation (up to 20% per genome) from best half of the population + some new random neural networks

Mutation:
- change a sign
- random gaussian number (sd = 1, 2, 10, 100)
- add random gaussian number (sd = 1, 2, 10, 100)
- multiply by 0.5, 0.9, 1.1 or 2.0

Crossover: one-point

### Configuration

```clojure
(def environment-config
  {:time-limit 10.0 ;; how much seconds to collect a dot
   :time-mult 3.0   ;; time penalty multiplier
   :dots-count 10   ;; dots to collect
   :rot-mult -0.2    ;; rotation penalty multiplier
   })

(def car-config
  {:max-velocity 15.0 ;; max speed
   :max-acceleration-f 1.5 ;; max acc forward
   :max-acceleration-b 0.7 ;; max acc backward
   :max-rotation 0.05 ;; max rotation speed in radians
   :friction-factor 0.95 ;; amount of speed value kept each frame (1.0 - no friction, 0.9 - high friction)
   })

(def simulation-config
  {:envrionments-count 1 ;; how many distinct environments use to train
   :profile [3 5 5 2] ;; network profile: 3 inputs, 2 outputs + hidden layers (5,5)
   :population-size 600 ;; how many neural networks should play the game
   :elite-percent 0.1 ;; how many best (percent)  neural networks should be used in the next gen
   :environment-config environment-config ;; default environment
   :car-config car-config ;; default car
   })
```

Run simulation (50 generation):

`(def sim (sim/run-all-games-n (sim/init-state simulation-config) 50))`

Continue simulation (next 50 generations)

`(def sim (sim/run-all-games-n sim 50))`

Continue simulation and change environments to new ones (to overcome overfitting)

`(def sim (sim/run-all-games-n sim 50 true))`

Build test environemnt

`(def some-env (env/environment WINDOW-SIZE environment-config car-config))`

Visualize the best car

`(nn-play sim)`

Visualize the worst car

`(nn-play sim nil (dec (:population-size sim)))`

Visualize the best car on fresh environement
 
`(nn-play sim some-env)`

![](results/neuroevolution/success.png)
