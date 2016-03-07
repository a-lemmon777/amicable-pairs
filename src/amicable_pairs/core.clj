(ns amicable-pairs.core)
(require '[clojure.core.reducers :as r])

(defn sum-complementary-divisors
  "If a divisor is a perfect square, returns the divisor.
  Otherwise returns the sum of the two complementary divisors."
  [num divisor]
  (let [dividend (/ num divisor)]
    (if (= divisor dividend)
      divisor
      (+ divisor dividend))))

(defn sum-divisors
  "Sums up all divisors for a number."
  [num]
  (let [sqrt (Math/sqrt num)
        lower-divisibles (r/filter #(zero? (mod num %)) (range 1 (inc (int sqrt))))
        last-piece (- (r/fold + (r/map #(sum-complementary-divisors num %) lower-divisibles)) num)]
    last-piece))

(defn amicable-pair?
  "Checks if two numbers are amicable (the sum of each number's divisors is equal to the other number)."
  [pair]
  (if (>= (first pair) (second pair))
    false
    (= (sum-divisors (second pair)) (first pair))))

(defn find-amicable-pairs
  "Returns the first num-pairs of amicable pairs."
  [num-pairs]
(let [pairs (pmap #(vector % (sum-divisors %)) (range))]
  (r/foldcat (r/take num-pairs (r/filter #(amicable-pair? %) pairs)))))

(time (find-amicable-pairs 20)
)
