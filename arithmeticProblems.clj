;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.

(defn is-prime [n]
  (cond
     (= n 1) false
     (or (= n 2) (= n 3)) true
     (= 0 (mod n 2)) false
     (= 0 (mod n 3)) false
     (empty? (filter #(= 0 (mod n %1)) (range 5 (inc (Math/ceil (Math/sqrt n))) 2))) true
     :else false))

(defn gcd [x y]
  (loop [a x b y]
    (if (= b 0)
      a
      (recur b (mod a b)))))

(defn is-coprime [a b]
  (= (gcd a b) 1))

(defn totient-naive [n]
  (count (filter (partial is-coprime n) (range 1 n))))

