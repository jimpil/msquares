;; https://en.wikipedia.org/wiki/Magic_square#Method_for_constructing_a_magic_square_of_odd_order

(ns msquares.odd
  (:require [msquares.util :as ut]))

;; ==================<ODD SQUARES>================================

(defn- magic?
  "Validates whether aN odd square is magic. Returns true/false."
  [square]
  (let [n (count square)
        M (ut/magic-constant n)
        last-number (ut/last-no n)
        middle-number (ut/middle-no n)
        [mi mj] (repeat 2 (long (/ n 2)))
        Mtest (fn [row-or-column]
                (= M (apply + row-or-column)))]
    (and (every? Mtest square)                ;; test rows sum
         (every? Mtest (ut/transpose square)) ;; test columns sum
         (every? Mtest (ut/diagonals square)) ;; test diagonals
         (= middle-number (get-in square [mi mj]))     ;; test middle number
         (= last-number (get-in square [(dec n) mj]))  ;; test last number
         )))

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
   :post [(magic? %)]}

  (let [mrange (range 1 (inc n))
        square (mapv (fn [i]
                       (mapv (partial fill n i) mrange))
                     mrange)]
    square
    )
  )

