(ns kmi.fpj.lecture06.lecture06_03)




(defn unique-random-numbers [n]
  (if (> n 0)
    (conj (unique-random-numbers (dec n)) (rand-int 100000))))

(defn myMin [x y]
  (if (< x y)
    x
    y))

(defn pmin [xs]
  (if (next (next xs))
    (let [a (future (myMin (nth xs 0) (pmin (next xs))))]
      (deref a))
    (myMin (nth xs 0) (nth xs 1))))

(defn pmin2 [xs]
  (if (> (count xs) 2)
    (let [newxs (split-at (int (/ (count xs) 2)) xs)
          a (future (pmin2 (nth newxs 0)))
          b (future (pmin2 (nth newxs 1)))]
      (apply min (list @a @b)))
    (apply min xs)))


(defn pmin3
  ([n] (pmin3 n 2))
  ([n level]
   (cond
     (<= level 0) (apply min n)
     :else (let [newxs (split-at (int (/ (count n) 2)) n)
                 a (future (pmin3 (nth newxs 0) (dec level)))
                 b (future (pmin3 (nth newxs 1) (dec level)))]
             (apply min (list @a @b))))))

