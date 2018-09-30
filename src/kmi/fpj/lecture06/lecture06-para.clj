;;
;; zpozdeni vypoctu
;;

(def a (delay (println "zpozdeny vypocet") 42))
(def b (delay (println "druhy vypocet") 11))

;; ziskani hodnoty
(deref a)

;; overeni pritomnosti hodnoty
(realized? a)
(realized? b)

;; zkratka v readeru
(+ @a @b)

;;
;; presunuti vypoctu do samostatneho vlakna
;;

(def f (future
         (Thread/sleep 10000)
         (println "jiny vypocet")
         666))

(deref f)

;;
;; prisliby jako forma synchronizace
;;

(def p (promise))
; (deliver p 42)

(def fut (future
  (println "Cekam na hodnotu")
  (let [r (+ @p 1)]
    (println "Konecne!")
    r)))

(deliver p 42)
(deref fut)
(deref p)

;;
;; Paralelni vypocet Fibonacci cisla
;;

(defn fib [n]
  (println "byla jsem zavolana")
  (if (< n 2)
    n
    (+ (fib (- n 1))(fib (- n 2)))))

(defn pfib
  "Parallel Fibonacci"
  ([n] (pfib n 2))
  ([n level]
   (cond
     (<= level 0) (fib n)
     (< n 2) n
     :else (let [a (future (pfib (- n 1) (dec level)))
                 b (future (pfib (- n 2) (dec level)))]
  (+ @a @b)))))

(time (fib 35))
(time (pfib 35))

;;
;; Paralelni QuickSort
;;

(defn rnd-seq [limit]
  "vraci sekvenic nahodnych cisel"
  (lazy-seq (cons (rand-int limit)
                  (rnd-seq limit))))

(def s (take 100000 (rnd-seq 100000)))

(defn qsort [xs]
  (if (<= (count xs) 1) xs
    (let [pivot (last xs)
          but-last (pop xs)
          lo (qsort (filterv (partial >= pivot) but-last))
          hi (qsort (filterv (partial < pivot) but-last))]
      ;(into (conj lo pivot) hi))
      (-> lo (conj pivot) (into hi)))))

(defn pqsort [xs level]
  (if (<= (count xs) 1) xs
    (let [pivot (last xs)
          but-last (pop xs)
          parallel (> level 0)
          lo-expr (delay (pqsort (filterv (partial >= pivot) but-last) (dec level)))
          hi-expr (delay (pqsort (filterv (partial < pivot) but-last) (dec level)))
          lo (if parallel (future @lo-expr) lo-expr)
          hi (if parallel (future @hi-expr) hi-expr)]
      (-> @lo (conj pivot) (into @hi)))))

(def ss (apply vector s))
(time (count (qsort ss)))
(time (count (pqsort ss 0)))
(time (count (pqsort ss 2)))

