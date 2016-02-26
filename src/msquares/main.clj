(ns msquares.main
  (:require [msquares
             [odd :as od]
             [even :as ev]
             [util :as ut]]
            [clojure.pprint :as pretty])
  (:gen-class))


(defn- produce-appropriate-square [n]
  (cond
    (odd? n)                 ["Odd" (od/odd-msquare n)]
    (and (even? n)
         (ut/multiple? n 4)) ["Doubly-even" (ev/doubly-even-msquare n)]
    (even? n)                ["Singly-even" (ev/singly-even-msquare n)]
    :else
    (throw (IllegalArgumentException. "Wrong input! Need a positive integer..."))))

(defn -main [& args]
  (time
    (let [n (Long/parseLong (first args))
          M (long (ut/magic-constant n))
          [order sq] (produce-appropriate-square n)
          columns (range 1 (inc n))]
      (println order "magic square")
      (println "Magic Constant:" M)
      (println "========================")
      (pretty/print-table (map #(into (sorted-map)
                                      (map vector columns %))
                               sq))
      (println)
      )
    )
  )
