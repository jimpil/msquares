(ns msquares.even
  (:require [msquares.odd :as odd]
            [msquares.util :as ut])
  (:import (java.util.concurrent.atomic AtomicLong)))
;;http://www.1728.org/magicsq2.htm

(defn- magic?
  "Validates whether a square is magic. Returns true/false."
  [square]
  (let [n (count square)
        M (ut/magic-constant n)
        Mtest (fn [row-or-column]
                (= M (apply + row-or-column)))]
    (and (every? Mtest square)                ;; test rows sum
         (every? Mtest (ut/transpose square)) ;; test columns sum
         (every? Mtest (ut/diagonals square)) ;; test diagonals sum
         )))


(defn- initial-step [n]
  (let [ys-no (/ n 4)
        xs-no (/ n 2)
        top-rows-seq (partition n (range 1 (inc (* ys-no n))))
        top-rows-filled (map (fn [row]
                               (into (vec (repeat ys-no nil))
                                     (apply conj (->> row
                                                      (drop ys-no)
                                                      (drop-last ys-no)
                                                      vec)
                                            (repeat ys-no nil))))
                             top-rows-seq)
        offset1 (inc (apply + (map count top-rows-filled)))
        middle-rows-seq (partition n (range offset1 (inc (+ offset1 (* xs-no n)))))
        middle-rows-filled (map (fn [row]
                                  (into (vec (take ys-no row))
                                        (apply conj (vec (repeat xs-no nil))
                                               (take-last ys-no row))))
                                middle-rows-seq)
        offset2 (+ offset1 (apply + (map count middle-rows-filled)))
        bottom-rows-seq (partition n (range offset2 (+ offset2 (* ys-no n))))
        bottom-rows-filled (map (fn [row]
                                  (into (vec (repeat ys-no nil))
                                        (apply conj (->> row
                                                         (drop ys-no)
                                                         (drop-last ys-no)
                                                         vec)
                                               (repeat ys-no nil))))
                                bottom-rows-seq)]
    (reduce into []
            [top-rows-filled middle-rows-filled bottom-rows-filled])
    )
  )

(defn doubly-even-msquare [n]
  {:pre  [(pos? n)
          (ut/multiple? n 4)]
   :post [(magic? %)]}

  (let [isquare (initial-step n)
        idx (AtomicLong. (* n n))]
    (mapv vec
          (partition n (for [i (range n)
                             j (range n)]
                         (if-let [slot (get-in isquare [i j])]
                           (do (.getAndDecrement idx) slot)
                           (.getAndDecrement idx)))))
    )
  )
;;=================================================================
(defn- shift-left-side [n srowsA srowsD]
  [(map (fn [depth rowA rowD]
          (reduce (fn [row idx]
                    (update (vec row) idx (constantly (nth rowD idx))))
                  rowA
                  (range depth)))
        (repeat (long (/ n 2)))
        (-> srowsA first drop-last)
        (-> srowsD first drop-last))

   [(reduce (fn [row idx]
              (update (vec row) idx (constantly (nth (-> srowsD first last) idx))))
            (-> srowsA first last)
            (range 1 (inc (long (/ n 2)))))]

   (map (fn [depth rowA rowD]
          (reduce (fn [row idx]
                    (update (vec row) idx (constantly (nth rowD idx))))
                  rowA
                  (range depth)))
        (repeat (long (/ n 2)))
        (last srowsA)
        (last srowsD))
   ])

(defn- shift-from-end [n depth X Y]
  (map (fn [depth rowX rowY]
         (reduce (fn [row idx]
                   (update (vec row) idx (constantly (nth rowY idx))))
                 rowX
                 (range (- n depth) n)))
       (repeat depth)
       X
       Y))

(defn- shift-right-side [n depth X Y]
  (let [newX (shift-from-end n depth X Y)
        newY (shift-from-end n depth Y X)]
    [newX newY]))

(defn singly-even-msquare
  [n]
  {:pre  [(>= n 6)
          (not (ut/multiple? n 4))
          (ut/multiple? n 2)]
   :post [(magic? %)]}

  (let [sq-n (/ n 2)
        sq-n-sq (* sq-n sq-n)
        adder (partial + sq-n-sq)
        oddA (odd/odd-msquare sq-n)
        oddB (map #(map adder %) oddA)
        oddC (map #(map adder %) oddB)
        oddD (map #(map adder %) oddC)

        srowsA (map vec (split-at (/ sq-n 2) oddA))
        srowsD (map vec (split-at (/ sq-n 2) oddD))
        srowsC-B-depth (when (> sq-n 3)
                         (some (fn [[idx odd]]
                                 (when (= odd sq-n)
                                   (inc idx)))
                               (map-indexed list (iterate (partial + 2) 5))))
        A (shift-left-side sq-n srowsA srowsD)
        D (shift-left-side sq-n srowsD srowsA)
        [final-A final-D] (map (partial reduce into []) [A D])
        [C B :as CB] [(if srowsC-B-depth
               (shift-right-side sq-n srowsC-B-depth oddC oddB )
               [oddC])
             (if srowsC-B-depth
               (shift-right-side sq-n srowsC-B-depth oddB oddC)
               [oddB])]
        [final-C final-B] (map (partial reduce into []) CB)
        top-half (mapv (comp vec concat) final-A final-C)
        bottom-half (map (comp vec concat) final-D final-B)
        complete (apply conj top-half bottom-half)]
    complete
    )
  )
