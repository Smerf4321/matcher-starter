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

(def testgrid '[[0 0]
                [0 0]])

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

(defn move-valid? [target grid]
    (and (<= 0 (first target))
         (<= 0 (last target))
         (< (first target) (count grid))
         (< (last target) (count (nth grid 0)))
         (= 0 (nth (nth grid (first target)) (last target)))))

(defn move [direction cord multiplier grid]
  (if (move-valid? (move-add direction cord multiplier) grid)
    (move-add direction cord multiplier)
    0
  ))

(defn lmg [details]
  (let [facing (nth details 0)
        current (nth details 1)
        grid (nth details 2)]
    (remove #(= 0 (last %))
            (list (list facing (move (cardinals (keyword facing)) current 1 grid))
                  (list facing (move (cardinals (keyword facing)) current 2 grid))
                  (list facing (move (cardinals (keyword facing)) current 3 grid))
                  (list 'north current grid)
                  (list 'east current grid)
                  (list 'south current grid)
                  (list 'west current grid)))
    ))

(defn path
  ([details]
   (let [current (list (nth details 0) (nth details 1) (nth details 3))
        target (list (nth details 0) (nth details 2) (nth details 3))]
    (breadth-search current target lmg)))

  ([facing current target grid]
   (let [current (list facing current grid)
         target (list facing target grid)]
     (breadth-search current target lmg)
     )))
