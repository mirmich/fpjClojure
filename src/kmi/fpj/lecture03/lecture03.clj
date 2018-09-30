(ns lecture03)

(defn fib [n]
  (let [iter (fn [a b i](if(<= i 0)
                          a
                          (recur b (+ a b) (- i 1))))
        _ (println n " th Fib number is" (iter 0 1 n))]
    (iter 0 1 n)))

(defn rpn-calc
  [l]
  (let [in? (fn
              [elm]
              (some #(= elm %) '(+ * / -)))

        push (fn
               [elm item]
               (concat (list elm) item))

        rp (fn
             [n st]
             (cond
               (not (first n)) st
               (in? (first n)) (recur (rest n)
                                      (push (apply (resolve (first n))
                                                   (list (first st) (second st)))
                                            (rest (rest st))))
               (not (in? (first n))) (recur (rest n) (push (first n) st))))]

    (rp l '())))

(fib 7)
(rpn-calc '(10 20 30 + *))
(rpn-calc '(10 20 + 30 *))
