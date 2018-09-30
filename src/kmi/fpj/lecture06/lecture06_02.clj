(ns kmi.fpj.lecture06.lecture06-02)

(defmulti inf (fn [a b]
                (cond (number? a) :integer
                      (set? a) :set)))

(defmethod inf :integer [a b]
  (min a b))

(defmethod inf :set [a b]
  (clojure.set/intersection a b))

(inf 4 5)
(inf #{1 2 3} #{3 4 5})
(inf #{'a 'b 'c} #{'b 'c 'd})