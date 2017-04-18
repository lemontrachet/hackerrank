(ns hackerrank.4clojure)

; implement range

; (= (__ -2 2) '(-2 -1 0 1))

(defn r
  [start end]
  (loop [n start
         acc '()]
    (if (= n end)
      (reverse acc)
      (recur (inc n) (conj acc n)))))


; flatten sequence
; (= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))


(fn [l]
     (into '()
           (loop [x l acc '()]
             (if (empty? x)
               acc
               (if (coll? (first x))
                 (recur (first x) acc)
                 (recur (rest x) (conj acc (first x))))))))
    '((1 2) 3 [4 [5 6]])


; replicate



; split at

; (= (__ 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])

(fn [n x]
  (let [first
          (take n x)
        second
          (drop n x)]
    (vector first second)))

; rotate sequence

(defn rotate-seq
  [n x]
  (let [length (count x)
        m (mod n length)]
    (cond
      (> n length) (concat
                     (drop m x)
                     (take m x))
      (< n (* -1 length)) (concat
                            (drop (- length (* -1 (mod (* -1 n) length))) x)
                            (take (- length (* -1 (mod (* -1 n) length))) x))
      (< 0 n length) (concat
                       (drop n x)
                       (take n x))
      (< n 0) (concat
                (drop (- length (* -1 n)) x)
                (take (- length (* -1 n)) x)))))



; destructuring

;[1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] __] [a b c d]))

; infix calc

(defmacro infix
  [f]
  (list (second f) (first f) (last f)))

; (= 7  (__ 2 + 5))



