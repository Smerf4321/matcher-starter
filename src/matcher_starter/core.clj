(ns matcher-starter.core
  (:require [org.clojars.cognesence.breadth-search.core :refer :all]
            [org.clojars.cognesence.matcher.core :refer :all]
            [org.clojars.cognesence.ops-search.core :refer :all]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(use 'clojure.test)

(def grid1 '[
           [0 0 0 0 0 0 1 0 0 0]
           [0 0 0 0 0 0 0 0 1 0]
           [0 0 0 1 0 0 0 0 0 0]
           [0 0 1 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 1 0 0 0]
           [0 0 0 0 0 1 0 0 0 0]
           [0 0 0 1 1 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0]
           [1 0 0 0 0 0 0 0 1 0]])

(def grid-result1 '[
                       [0 0 0 0 0 1 1 1 1]
                       [0 0 1 1 0 0 0 1 1]
                       [0 1 1 1 0 0 0 0 0]
                       [0 1 1 0 0 1 1 0 0]
                       [0 0 0 0 1 1 1 0 0]
                       [0 0 1 1 1 1 0 0 0]
                       [0 0 1 1 1 0 0 0 0]
                       [1 0 0 0 0 0 0 1 1]
                       ])


(def grid2 '[
             [0 0 0]
             [0 0 0]
             [0 1 0]
             [0 0 0]
             ])

(def grid-result2 '[
                    [0 0]
                    [1 1]
                    [1 1]
                    ])

(def cardinals '{:north (1 0), :south (-1 0), :east (0 1), :west (0 -1)})

(def d '(north (2 2) (5 5) grid1))

(defn translate-grid
  [grid]
  (loop [grid grid x '0 y '0 translated '[[]]]
    (cond
      (== y (- (count grid) 1))
        (pop translated)
      (== x (- (count (nth grid y)) 1))
        (recur grid 0 (+ y 1) (conj translated '[]))
      (and
        (== (nth (nth grid y) x) 0)
        (== (nth (nth grid y) (+ x 1)) 0)
        (== (nth (nth grid (+ y 1)) x) 0)
        (== (nth (nth grid (+ y 1)) (+ x 1)) 0))
          (recur grid (+ x 1) y
                 (conj (pop translated) (conj (last translated) 0)))
      :else
        (recur grid (+ x 1) y (conj (pop translated) (conj (last translated) 1)))
      )
    )
  )

(deftest translate-grid-test
         (is (== (compare (translate-grid grid1) grid-result1) 0))
         (is (== (compare (translate-grid grid2) grid-result2) 0))
         )


(defn turn [direction]
  (cond
    (= direction 'north)
    'east
    (= direction 'east)
    'south
    (= direction 'south)
    'west
    (= direction 'west)
    'north))

(defn move [direction cord]
  (list (+ (first direction) (first cord)) (+ (last direction) (last cord))))

(defn lmg [details]
  (let [direction (nth details 0)
       current (nth details 1)
       target (nth details 2)
       grid (nth details 3)]
    
    (list (list direction (move (cardinals (keyword direction)) current) target grid) 
      (list (turn direction) current target grid))
))

(defn path [direction current target grid]
  (breadth-search (direction current target grid)))
