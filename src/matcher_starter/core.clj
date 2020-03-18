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

;; this function translates the grid which was passed by user to a grid which represents locations where robot can be
;; and not. Essentially this function scales the map so the robot only is taking a single spot rather than 4.
;; NOTE: the function was only tested with 2D Vectors. There is no guarantee it will work for other list likes.
;; @arg grid a 2 dimentional vector list which represents the world.
;; @return translated grid which represents locations at which robot can be and not be.
(defn translate-grid
  [grid]
  (loop [grid grid x '0 y '0 translated '[[]]]
    (cond
      (== y (- (count grid) 1))                       ;; there is no more rows to process
      (pop translated)
      (== x (- (count (nth grid y)) 1))               ;; This row has finished being processed
      (recur grid 0 (+ y 1) (conj translated '[]))    ;; Add space for next row and repeat
      (and                                            ;; Check if all 4 places allow the robot to be in them
        (== (nth (nth grid y) x) 0)
        (== (nth (nth grid y) (+ x 1)) 0)
        (== (nth (nth grid (+ y 1)) x) 0)
        (== (nth (nth grid (+ y 1)) (+ x 1)) 0))
      (recur grid (+ x 1) y                           ;; Go to next column, and add valid position
             (conj (pop translated) (conj (last translated) 0)))
      :else                                           ;; The position is not valid, mark it as such
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

(defn long-move-valid [target direction grid multiplier]
  (cond
    (= 1 multiplier)
      (= 0 (nth (nth grid (first target)) (last target)))
    (= 2 multiplier)
        (and
           (= 0 (nth (nth grid (- (first target) (first direction))) (- (last target) (last direction))))
           (= 0 (nth (nth grid (first target)) (last target))))
    (= 3 multiplier)
        (and
           (= 0 (nth (nth grid (- (first target) (* 2 (first direction)))) (- (last target) (* 2 (last direction)))))
           (= 0 (nth (nth grid (- (first target) (first direction))) (- (last target) (last direction))))
           (= 0 (nth (nth grid (first target)) (last target))))))

(defn move-valid? [target direction grid multiplier]
  (and (<= 0 (first target))
       (<= 0 (last target))
       (< (first target) (count grid))
       (< (last target) (count (nth grid (first target))))
       (long-move-valid target direction grid multiplier)
       ))

(defn move [direction cord multiplier grid]
  (if (move-valid? (move-add direction cord multiplier) direction grid multiplier)
    (move-add direction cord multiplier)
    cord
  ))

(defn lmg [details]
  (let [facing (nth details 0)
        current (nth details 1)
        grid (nth details 2)]
            (list (list facing (move (cardinals (keyword facing)) current 1 grid) grid)
                  (list facing (move (cardinals (keyword facing)) current 2 grid) grid)
                  (list facing (move (cardinals (keyword facing)) current 3 grid) grid)
                  (list 'north current grid)
                  (list 'east current grid)
                  (list 'south current grid)
                  (list 'west current grid))
    ))

(defn finish? [current target]
  (= (nth current 1) target))

(defn path
  [facing current target grid]
   (let [start (list facing current grid)
         finish (list facing target grid)]
     (- (count (breadth-search start #(finish? % target) lmg)) 1)))

(defn path-brute-force [facing current target grid]
  (path facing (map dec current) (map dec target) (translate-grid grid))
)


(defn path-dijkstra [facing current target grid]

)