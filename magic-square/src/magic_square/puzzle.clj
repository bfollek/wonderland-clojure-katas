(ns magic-square.puzzle
  "wonderland magic-square kata. Algorithm from
   https://www.geeksforgeeks.org/magic-square/"
  (:require [clojure.math.numeric-tower :as math]))

(def values
  "Test values."
  [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn- first-pos
  "First position to use."
  [n]
  ;; First pos is [n/2, n-1].
  (let [row (quot n 2)
        col (dec n)]
    [row col]))

(defn- adjust-pos
  "Handle the edge cases."
  [n [row col]]
  ;; If row = -1 & col = n, next pos is (0, n-2).
  (if (and (= row -1) (= col n))
    [0, (- n 2)]
    ;; If row = -1, wrap around to n-1.
    ;; If col = n, wrap around to 0.
    (let [row (if (= row -1) (dec n) row)
          col (if (= col n) 0 col)]
      [row col])))

(defn- next-pos
  "Calculate the next position to use."
  [n pos-so-far]
  (let [[last-row last-col] (last pos-so-far)
        row (dec last-row)  col (inc last-col)]
    (loop [row row col col]
      (let [[row col] (adjust-pos n [row col])]
        ;; If we've already used the calculated position,
        ;; increment row and decrement col by 2, and try again.
        (if (some #(= [row col] %) pos-so-far)
          (recur (inc row) (- col 2))
          [row col])))))

(defn- get-all-pos
  "Return all the positions, in value order: first value goes in the first
  postion returned, second value goes in the second, etc. Vector of vectors."
  [n]
  (loop [pos-so-far [(first-pos n)]]
    (if (> (math/expt n 2) (count pos-so-far))
      (recur (conj pos-so-far (next-pos n pos-so-far)))
      pos-so-far)))

(defn- pos-values-map
  "Return a map of [x,y] positions to values.
  Looks like {[2 2] 4.5 [0 0] 1.5} etc."
  [n values]
  (->> (sort values)
       (map vector (get-all-pos n))
       (into {})))

(defn- format-result
  "Position-to-value map in, nXn vector of vectors of values out."
  [n pos-map]
  (->> (for [x (range 0 n)
             y (range 0 n)] (pos-map [x y]))
       (partition n)
       (mapv vec)))

(defn magic-square
  "Arrange a collection of values in a magic square:
  All rows, columns, and diagonals have the same sum."
  [values]
  (let [n (math/sqrt (count values)) ; n = number of squares per row
        pv-map (pos-values-map n values)]
    (format-result n pv-map)))
