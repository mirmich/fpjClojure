(ns kmi.fpj.lecture04.lecture04)
;;;;;;;;UKOL 1;;;;;;;;;;;;;;;;;;;;;;;;
(defn fib [n]
  (let [iter (fn [a b i](if(<= i 0)
                          a
                          (recur b (+ a b) (- i 1))))
        _ (println n " th Fib number is" (iter 0 1 n))]
    (iter 0 1 n)))

(defn fib-seq []
  ((fn fib-in [n]
     (lazy-seq (cons (fib n)
                     (fib-in (inc n)))))0))


;;;;;;;;UKOL 2;;;;;;;;;;;;;;;;;;;;;;;;

(defn rpn-calc
  [expr env]
  (let [isOp? (fn                                             ;;
              [elm]
              (some #(= elm %) '(+ * / -)))

        push (fn                                            ;;funkce pro push na zasobnik
               [elm item]
               (concat (list elm) item))

        rp (fn
             [n st]
             (cond
               (not (first n)) st                           ;; konec
               (isOp? (first n)) (recur                       ;; uplatni op a pushne vysledek
                                 (rest n)
                                 (push
                                   (apply (resolve (first n))(list (first st) (second st)))
                                   (rest (rest st))))
               (not (isOp? (first n))) (if (contains? env (first n)) ;; neni operator
                                          (recur (rest n) (push (get env (first n)) st)) ;; zeslovniku
                                          (recur (rest n) (push (first n) st)))))] ;; neni ve slovniku
    (rp expr '())))


(rpn-calc '(10 a 30 + *) (hash-map 'a 20))
(rpn-calc '(10 a 30 + *) (hash-map 'a 20,'b 10))



;;;;;;;;UKOL 3;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "src/kmi/fpj/lecture04/countries.clj")
kmi.fpj.lecture04.countries/countries
(def countries kmi.fpj.lecture04.countries/countries)

;; names of states
(map :name countries)

;; list of continents
(distinct (map :continent countries))

;; govertmentforms and occurencies
(into {} (map
           (fn [x] [(x 0) (count (x 1))])
           (group-by :governmentform countries)))

(apply max (map :population countries))

;; count states on continents
(map
  (fn [x] [(x 0) (count (x 1))])
  (group-by :continent countries))

;; most populated country
(last (sort-by :population countries))

;; average population over countries
(biginteger (/ (apply + (map :population countries))
               (count (map :population countries))))





