(ns sicp-clojure.ch1 
  (:require [sicp-clojure.util :refer [trace]]))

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

;; 1.1.7 newton's method
(defn average [x y]
  (/ (+ x y) 2))
(defn improve [guess x]
  (average guess (/ x guess)))
(defn good-enough? [guess x]
  (->
   (sq guess)
   (- x)
   abs
   (< 0.0001)))
(defn sqrt-iter [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))
(defn sqrt [x]
  (sqrt-iter 1.0 x))

;; 1.6
(defn new-if-1-6 [predicate then else]
  (cond predicate then
        :else else))
(defn sqrt-iter-1-6 [guess x]
  (new-if-1-6 (good-enough? guess x)
              guess
              (sqrt-iter-1-6 (improve guess x) x)))
;; will stack overflow as the two cond is not layily evaluated

;; 1.7
(defn good-enough-1-7? [guess x]
  (->
   (sq guess)
   (- x)
   abs
   (< (/ x 1E5))))

;; 1.8
(defn improve-1-8 [y x]
  (->
   (/ x (sq y))
   (+ (* 2 y))
   (/ 3)))
(defn good-enough-1-8? [guess x]
  (->
   (* guess guess guess)
   (- x)
   abs
   (< (/ x 1E5))))
(defn cube-root-iter-1-8 [guess x]
  (println guess x)
  (if (good-enough-1-8? guess x)
    guess
    (cube-root-iter-1-8 (improve-1-8 guess x) x)))
(defn cube-root-1-8 [x]
  (cube-root-iter-1-8 1.0 x))
(cube-root-1-8 27)

(defn fact [n]
  (if (> n 0)
    (do
      (defn rec-core [acc x]
        (if (= x 1)
          acc
          (rec-core (* acc x) (- x 1))))
      (rec-core 1 n))
    (println "fact cannot accept n <= 0")))
(fact 6)

;; 1.10
(defn A-1-10 [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (->>
               (- y 1)
               (A-1-10 x)
               (A-1-10 (- x 1)))))
(A-1-10 1 10)
(A-1-10 2 4)
(A-1-10 3 3)

(defn f-1-10 [n] (A-1-10 0 n)) ;; 2n
(defn g-1-10 [n] (A-1-10 1 n)) ;; 2^n
(defn h-1-10 [n] (A-1-10 2 n)) ;; 2^h(n-1), h(1) = 2
(defn k-1-10 [n] (* 5 n n))

(defn fib [n]
  (defn core [a b count]
    (if (= count 0)
      b
      (core (+ a b) a (- count 1))))
  (core 1 0 n))

(defn count-change [amount]
  (defn fst-denomination [kinds-of-coins]
    (cond (= kinds-of-coins 1) 1
          (= kinds-of-coins 2) 5
          (= kinds-of-coins 3) 10
          (= kinds-of-coins 4) 25
          (= kinds-of-coins 5) 50))
  (defn cc [amount kinds-of-coins]
    (cond (= amount 0) 1
          (or (< amount 0) (= kinds-of-coins 0)) 0
          :else (+ (cc amount (- kinds-of-coins 1))
                   (cc (- amount
                          (fst-denomination kinds-of-coins))
                       kinds-of-coins))))
  (cc amount 5))
(count-change 100)

;; 1.11
(defn fn-1-11 [n]
  (defn core-1-11 [fst snd thr x]
    (println fst snd thr x)
    (if (<= x 0)
      fst
      (core-1-11 (+ fst (* 2 snd) (* 3 thr))
                 fst
                 snd
                 (- x 1))))
  (if (< n 3)
    n
    (core-1-11 2 1 0 (- n 2))))
(defn fn-1-11-2 [n]
  (if (>= n 3)
    ;; (+ (fn-1-11-2 (- n 1))
    ;;    (* 2 (fn-1-11-2 (- n 2)))
    ;;    (* 3 (fn-1-11-2 (- n 3))))
    (->>
     [(fn-1-11-2 (- n 1)) (* 2 (fn-1-11-2 (- n 2))) (* 3 (fn-1-11-2 (- n 3)))]
     (map trace)
     (reduce + 0))
    n))
(fn-1-11 5)
(fn-1-11-2 5)

; 1.12 pascal triangle
(defn fn-1-12 [layer]
  (defn get-pos-num-1-12 [n current-layer prev-layer-ls]
    (if (or (= n 0) (= n (dec current-layer)))
      1
      (+ (prev-layer-ls (- n 1)) (prev-layer-ls n)))) 
  (defn core-1-12 [current-layer]
    (if (= current-layer 1)
    [1]
    (let [prev-layer (core-1-12 (dec current-layer))]
      (->>
       (for [n (range 0 current-layer)]
         (get-pos-num-1-12 n current-layer prev-layer))
       (into [])))))
  (->>
   (for [n (range 1 (inc layer))] (core-1-12 n))
   (into [])))
(fn-1-12 4)

;; 1.13
;; prove |Fib(n) - φ^n/sqrt(5)| <= 1, φ = (1+sqrt(5))/2
;; when n = 1
;; LHS = |Fib(1) - (1+sqrt(5))/(2*sqrt(5))|
;;     = |1 - 0.7236|
;;     <= RHS

;; assume |Fib(k) - φ^k/sqrt(5)| <= 1 is true
;; i.e. -1 <= Fib(k) - φ^k/sqrt(5) <= 1
;;      φ^k/sqrt(5) - 1 <= Fib(k) <= φ^k/sqrt(5) + 1

;; when n = k + 1
;; LHS = |Fib(k+1) - φ^(k+1)/sqrt(5)|
;;     <= |φ^(k+1)/sqrt(5) + 1 - φ^(k+1)/sqrt(5)|
;;     = 1

;; 1.15
(defn cube [x] (* x x x))
(defn p [x]
  (println "-")
  (- (* 3 x) (* 4 (cube x))))
(defn sine [angle]
  (if (not (> (abs angle) 0.1))
    angle 
    (p (sine (/ angle 3.0)))))
(sine 12.15)