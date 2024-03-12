(ns sicp-clojure.ch1)

(defn sq [x]
  (* x x))

;; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;; 1.3
(defn ex1-3 [a b c]
  (let [min-value (min a b c)]
    (cond (= min-value a) (* (sq c) (sq b))
          (= min-value b) (* (sq a) (sq c))
          (= min-value c) (* (sq a) (sq b)))))
(ex1-3 1 2 3)

;; 1-4
