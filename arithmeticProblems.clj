;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.

(defn get-extra-factors [n]
  (range 5 (inc (Math/ceil (Math/sqrt n))) 2))

(defn is-prime [n]
  (cond
     (= n 1) false
     (or (= n 2) (= n 3)) true
     (= 0 (mod n 2)) false
     (= 0 (mod n 3)) false
     (empty? (filter #(= 0 (mod n %1)) (get-extra-factors n))) true
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


;; Code to get greatest prime factor
(defn remove-factor [n factor]
  (loop [m n]
    (if (= (mod m factor) 0)
      (recur (/ m factor))
      m)))

(defn greatest-prime-factor [n]
  (let [factors (conj (conj (get-extra-factors n) 3) 2)]
    (reduce #(remove-factor %1 %2) n factors)))

;; Code to get all prime factors

(defn prime-factors [n]
  (let [factors (conj (conj (range 5 (inc (/ n 2)) 2) 3) 2)]
    (filter (fn [x] (and (= 0 (mod n x)) (is-prime x)) ) factors)))

