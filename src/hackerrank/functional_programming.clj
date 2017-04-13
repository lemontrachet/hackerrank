(ns hackerrank.functional-programming
  (:require [clojure.string :as str]))

; pascal's triangle
; n! / (r! * (n - r)!)

(defn fact
  [n]
  (reduce * (range 1 (+ n 1))))

(defn pascal
  [row]

  (let [cols (+ row 1)
        current-row
        (into []
              (for [c (range cols)]

                (/ (fact row) (* (fact c) (fact (- row c))))
                )
              )
        ]
    (clojure.string/join " " current-row)
    )

  )

(defn draw-triangle
  [rows]
  (let [triangle
        (into []
              (for [i (range rows)]
                (pascal i)))]
    (println (clojure.string/join "\n" triangle))
    ))


; remove duplicates

(defn rem-sequential-dupes
  [s]
  (loop [[head & tail] (seq s)
         out []]
    (if (empty? tail)
      (clojure.string/join "" (conj out head))

      (if (not= head (first tail))
        (recur tail (conj out head))
        (recur tail out)))
  ))

(defn rem-dupes
  [s]

  (loop [[head & tail] (seq s)
         out []]
    (if (nil? head)
      (clojure.string/join "" out)
      (if (some #{head} out)
        (recur tail out)
        (recur tail (conj out head))
        ))))



; e^x

(defn to-power
  [x n]
  (reduce * (repeat n x))
  )

(defn e
  [x n]
  (/ (to-power x n) (fact n))
  )

(defn make-seq
  [x]
  (float (reduce + (into [1 x]
      (for [i (range 2 10)]
        (e x i)))))
  )


; merge strings

(defn mingle-str
  [s1 s2]
  (clojure.string/join "" (into []
  (for [[x y] (map vector (seq s1) (seq s2))]
    (clojure.string/join "" [x y])
    ))))

; rotate string

(defn rotate-str
  [s]

  (loop [word s
          n 0]
    (if (< n (count s))
      (let [[head & tail] (seq word)
            new (clojure.string/join "" (concat tail [head]))]
            (println (str new))
            (recur new (inc n))
        ))))

(println (rotate-str "hello"))