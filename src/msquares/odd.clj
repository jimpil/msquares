;; https://en.wikipedia.org/wiki/Magic_square#Method_for_constructing_a_magic_square_of_odd_order

(ns msquares.odd
  (:require [msquares.util :as ut]))

;; ==================<ODD SQUARES>================================

(defn- magic-odd?
  "Validates whether aN odd square is magic. Returns true/false."
  [square]
  (let [n (count square)
        last-number (ut/last-no n)
        middle-number (ut/middle-no n)
        [mi mj] (repeat 2 (long (/ n 2)))]
    (and (ut/magic? square) ;; test diagonals
         (= middle-number (get-in square [mi mj]))     ;; test middle number
         (= last-number (get-in square [(dec n) mj])))  ;; test last number
         ))

(defn- fill
  "The actual formula for populating each square in a 2D matrix of size <n>."
  [n i j]
  (+ (* n (mod (+ i j (Math/floorDiv (long n) (long 2)) -1)
               n))
     (mod (+ i (* 2 j) -2)
          n)
     1)
  )


(defn odd-msquare
  "Constructs magic-squares of (odd) order <n>."
  [n]
  {:pre  [(integer? n)
          (>= n 1)
          (odd? n)]
   :post [(magic-odd? %)]}

  (let [mrange (range 1 (inc n))
        square (mapv (fn [i]
                       (mapv (partial fill n i) mrange))
                     mrange)]
    square
    )
  )

