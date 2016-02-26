;; https://en.wikipedia.org/wiki/Magic_square#Method_for_constructing_a_magic_square_of_odd_order

(ns msquares.odd
  (:require [msquares.util :as ut]))

;; ==================<ODD SQUARES>================================

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
   :post [(ut/magic? %)]}

  (let [mrange (range 1 (inc n))
        square (mapv (fn [i]
                       (mapv (partial fill n i) mrange))
                     mrange)]
    square
    )
  )

