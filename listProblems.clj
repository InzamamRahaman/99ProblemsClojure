;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.

(def my-last
  (comp first reverse))

(defn penultimate [xs]
  (get (into [] (reverse xs)) 1))

(defn my-kth [k xs]
  (loop [[h & tail] xs n k]
    (if (= n 1)
      h
      (recur tail (- n 1)))))

(defn num-elements [xs]
  (if (empty? xs)
    0
    (loop [[h & tail] xs acc 1]
      (if (empty? tail) acc (recur tail (+ acc 1))))))

(defn my-num-elements2 [xs]
  (reduce (fn [acc x] (inc acc)) 0 xs))


(defn my-reverse [xs]
  (if (empty? xs) ()
    (loop [[h & tail] xs acc ()]
      (if (empty? tail)
        (conj acc h)
        (recur tail (conj acc h))))))

(defn my-rev [xs]
  (reduce (fn [acc x] (conj acc x)) (list) xs))


(defn is-palindrome [xs]
  (= xs (my-reverse xs)))

(defn collect [f xs]
  (->> xs (map f) (apply concat)))

;;(apply concat (map (partial repeat 3) [1 2 3]))

(defn flatten [xs]
  (if (list? xs) (collect flatten xs) (list xs)))

(defn compress [xs]
  (->> xs (partition-by identity) (map first)))

(defn pack [xs]
  (->> xs (partition-by identity)))

(defn encode [xs]
  (->> xs (partition-by identity)
       (map (fn [xs] (hash-map :elem (first xs) :len (num-elements xs))))))

(defn explode-encoding [{:keys [elem len]}]
  (repeat len elem))

(defn decode [xs]
  (->> xs (map explode-encoding)))

(defn my-replicate [n xs]
  (->> xs (collect (partial repeat n))))

(defn duplicate [xs]
  (->> xs (my-replicate 2)))

(defn my-drop [n xs]
  (->> xs (partition n) (collect (partial take (- n 1)))))

(defn my-slice [xs start end]
  (->> xs (drop start) (take (+ (- end start) 1))))












