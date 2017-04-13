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
    (str/join " " current-row)
    )

    )

(defn draw-triangle
  [rows]
  (let [triangle
        (into []
        (for [i (range rows)]
          (pascal i)))]
  (println (str/join "\n" triangle))
  ))

(draw-triangle 21)
