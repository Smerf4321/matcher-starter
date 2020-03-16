(ns matcher-starter.core
  (:require [org.clojars.cognesence.breadth-search.core :refer :all]
            [org.clojars.cognesence.matcher.core :refer :all]
            [org.clojars.cognesence.ops-search.core :refer :all]))


(use 'clojure.test)

(def n [0 1])
(def s [0 -1])
(def e [1 0])
(def w [-1 0])

(defn move [dir pos] (map + (dir pos)))

(defn robo [statt end grid])


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
