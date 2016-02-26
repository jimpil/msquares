(ns msquares.util)


(defn transpose [square]
  (apply map vector square))

(defn last-no
  "The biggest number in magic square of order <n>."
  [n]
  (* n n))

(defn middle-no
  "The number in the middle of the magic square of order <n>."
  [n]
  (-> (last-no n)
      inc
      (/ 2)))

(defn magic-constant
  "The magic-constant (M) of the magic square of order <n>."
  [n]
  (* (middle-no n) n))

(defn diagonals
  "Returns the 2 diagonals of a magic-square.
  Expecting vectors all the way in."
  [square]
  (let [n (count square)
        rows (range n)]
  [(map #(get-in square [% %]) rows)
   (map #(get-in square [% (- n (inc %))]) rows)]))

(defn multiple? [x y]
  (zero? (rem x y)))

(defn- sum [row-or-column-or-diagonal]
  (apply + row-or-column-or-diagonal))

(defn magic?
  "Validates whether a square is magic. Returns true/false."
  [square]
  (let [n (count square)
        M (magic-constant n)
        Mtest (comp (partial = M) sum)]
    (and (every? Mtest square)             ;; test rows sum
         (every? Mtest (transpose square)) ;; test columns sum
         (every? Mtest (diagonals square)) ;; test diagonals sum
         )))