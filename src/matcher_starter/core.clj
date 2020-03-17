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

(defn robo [start end grid])

(def map-grid1 {
                :00 [:01 :10]
                :01 [:00 :02 :11]
                :02 [:01 :03 :12]
                :03 [:02 :13]
                :10 [:00 :11 :20]
                :11 [:01 :10 :12 :21]
                :12 [:02 :11 :13 :22]
                :13 [:03 :12 :23]
                :20 [:10 :21 :30]
                :21 [:11 :20 :22 :31]
                :22 [:12 :21 :23 :32]
                :23 [:13 :22 :33]
                :30 [:20 :31]
                :31 [:21 :30 :32]
                :32 [:22 :31 :33]
                :33 [:23 :32]
                })

(defn d [from to]
  (let [d {:00 {:01 1 :10 2}
           :01 {:00 3 :02 1 :11 2}
           :02 {:01 3 :03 1 :12 2}
           :03 {:02 3 :13 2}
           :10 {:00 4 :11 1 :20 2}
           :11 {:01 4 :10 3 :12 1 :21 2}
           :12 {:02 4 :11 3 :13 1 :22 2}
           :13 {:03 4 :12 3 :23 2}
           :20 {:10 4 :21 1 :30 2}
           :21 {:11 4 :20 3 :22 1 :31 2}
           :22 (:12 4 :21 3 :23 1 :32 2)
           :23 {:13 4 :22 3 :33 2}
           :30 {:20 4 :31 1}
           :31 {:21 4 :30 3 :32 1}
           :32 {:22 4 :31 3 :33 1}
           :33 {:23 4 :32 3}
           }]))

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
