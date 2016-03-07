(ns amicable-pairs.core)
(require '[clojure.core.reducers :as r])

(defn handle-perfect-square
  ""
  [num divisor]
  (let [dividend (/ num divisor)]
    (if (= divisor dividend)
      divisor
      (+ divisor dividend))))

(defn sum-divisors
  ""
  [num]
  (let [sqrt (Math/sqrt num)
        lower-divisibles (r/filter #(zero? (mod num %)) (range 1 (inc (int sqrt))))
        last-piece (- (r/fold + (r/map #(handle-perfect-square num %) lower-divisibles)) num)]
    last-piece))

(defn amicable-pair?
  ""
  [pair]
  (if (>= (first pair) (second pair))
    false
    (= (sum-divisors (second pair)) (first pair))))

(defn find-amicable-pairs
  ""
  [num-pairs]
(let [pairs (pmap #(vector % (sum-divisors %)) (range))]
  (r/foldcat (r/take num-pairs (r/filter #(amicable-pair? %) pairs)))))

(time (find-amicable-pairs 20)
)

;; 7  1.5  .2  .2 .173 .15
;; 8
;; 9
;; 10  20  1   1  1.1 .8
;; 11
;; 12
;; 13
;; 14  62 2.4  2.3 2.7  1.6
;; 15
;; 16
;; 17
;; 18
;; 19
;; 20  5.5 minutes  4.2  4.0 5.0 3.1
