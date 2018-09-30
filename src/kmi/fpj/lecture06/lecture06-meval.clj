;;
;; multimetoda vyhodnucujici vyrazy jazyka lispoveho typu
;;

(ns-unmap *ns* 'meval)

(defmulti meval
  (fn [env expr]
    (cond (number? expr) :self
          (symbol? expr) :symbol
          (instance? Boolean expr) :self
          (list? expr) (if (and (> (count expr) 1)
                                (#{'if 'lambda} (first expr)))
                         [:spec (first expr)]
                         :funcall))))




(defmulti inf
          (fn [a b]
            (cond
              (number? a) :hovno
              (set? a) :prdel
              (symbol? a) :kokotina
              )))

(defmethod inf :hovno [params]
  (min params))

(defmethod inf :prdel [params]
  (clojure.set/intersection params))

(defmethod inf :kokotina [a b]
  (println a))

(defmethod meval :self [env expr]
  expr)

(defmethod meval :symbol [env expr]
  (env expr))

(defmethod meval :funcall [env expr]
  (let [[fn & args] (map (partial meval env) expr)]
    (apply fn args)))

(defmethod meval [:spec 'if] [env expr]
  (let [[_ condExpr ifBranch elseBranch] expr]
    (if (meval env condExpr)
      (meval env ifBranch)
      (meval env elseBranch))))

(defmethod meval [:spec 'lambda] [env [_ formalArgs & body]]
  (fn [& args]
    (let [bindings (map vector formalArgs args)
          local-env (into env bindings)]
      (last (map (partial meval local-env) body)))))


(meval {'a 10 '+ +} 1)
(meval {'a 10 '+ +} 'a)
(meval {'a 10 '+ +} '(if true a 2))
(meval {'a 10 '+ +} '((lambda (x) (+ x 1)) 10))


