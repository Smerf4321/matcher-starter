(ns matcher-starter.core
  (:require [org.clojars.cognesence.breadth-search.core :refer :all]
            [org.clojars.cognesence.matcher.core :refer :all]
            [org.clojars.cognesence.ops-search.core :refer :all]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(def grid [[0 0 0 0 0 0 1 0 0 0]
             [0 0 0 0 0 0 0 0 1 0]
             [0 0 0 1 0 0 0 0 0 0]
             [0 0 1 0 0 0 0 0 0 0]
             [0 0 0 0 0 0 1 0 0 0]
             [0 0 0 0 0 1 0 0 0 0]
             [0 0 0 1 1 0 0 0 0 0]
             [0 0 0 0 0 0 0 0 0 0]
             [1 0 0 0 0 0 0 0 1 0]])
