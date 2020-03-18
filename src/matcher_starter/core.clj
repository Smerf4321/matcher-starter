(ns matcher-starter.core
  (:require [org.clojars.cognesence.breadth-search.core :refer :all]
            [org.clojars.cognesence.matcher.core :refer :all]
            [org.clojars.cognesence.ops-search.core :refer :all]))

(def grid1 '[[0 0 0 0 0 0 1 0 0 0]
             [0 0 0 0 0 0 0 0 1 0]
             [0 0 0 1 0 0 0 0 0 0]
             [0 0 1 0 0 0 0 0 0 0]
             [0 0 0 0 0 0 1 0 0 0]
             [0 0 0 0 0 1 0 0 0 0]
             [0 0 0 1 1 0 0 0 0 0]
             [0 0 0 0 0 0 0 0 0 0]
             [1 0 0 0 0 0 0 0 1 0]])

(def grid-result1 '[[0 0 0 0 0 1 1 1 1]
                    [0 0 1 1 0 0 0 1 1]
                    [0 1 1 1 0 0 0 0 0]
                    [0 1 1 0 0 1 1 0 0]
                    [0 0 0 0 1 1 1 0 0]
                    [0 0 1 1 1 1 0 0 0]
                    [0 0 1 1 1 0 0 0 0]
                    [1 0 0 0 0 0 0 1 1]
                    ])

(def testgrid '[[0 0 0] [0 0 0] [0 0 0]])

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

(def cardinals '{:north (-1 0), :south (1 0), :east (0 1), :west (0 -1)})

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

(defn move-add [direction cord multiplier]
  (list (+ (* multiplier (first direction)) (first cord)) (+ (* multiplier (last direction)) (last cord))))

(defn move-valid? [target]
  (and (<= 0 (first target))
       (<= 0 (last target))
       (< (first target) (count grid1))
       (< (last target) (count (nth grid1 (first target))))
       (= 0 (nth (nth grid1 (first target)) (last target)))))

(defn move [direction cord multiplier]
  (if (move-valid? (move-add direction cord multiplier))
    (move-add direction cord multiplier)
    cord
  ))

(defn lmg [details]
  (let [facing (nth details 0)
        current (nth details 1)]
            (list (list facing (move (cardinals (keyword facing)) current 1))
                  (list facing (move (cardinals (keyword facing)) current 2))
                  (list facing (move (cardinals (keyword facing)) current 3))
                  (list 'north current)
                  (list 'east current)
                  (list 'south current)
                  (list 'west current ))
    ))

(defn path
  ([details]
   (let [current (list (nth details 0) (nth details 1))
        target (list (nth details 0) (nth details 2))]
    (breadth-search current target lmg)))

  ([facing current target]
   (let [current (list facing current)
         target (list facing target)]
     (breadth-search current target lmg)
     )))
