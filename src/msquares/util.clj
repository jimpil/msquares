(ns msquares.util)


(defn transpose [square]
  (apply map vector square))

(defn last-no
  "Is always opposite the number 1 in an outside column/row."
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