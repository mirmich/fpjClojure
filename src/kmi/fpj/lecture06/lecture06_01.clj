(ns kmi.fpj.lecture06.lecture06-01)




(defmulti isOn? :light-type)
(defmulti turnOn :light-type)
(defmulti turnOff :light-type)

(defmethod isOn? :default [x] (:state x))


;; Implementace PerfectLight
(defn light []
  {:light-type :Light :state false :broken false})

(defmethod turnOn :Light [l]
  {:light-type (:light-type l) :state true :broken false})

(defmethod turnOff :Light [l]
  {:light-type (:light-type l) :state false :broken false})


;;Implementace CheapLight
(defn cheapLight []
  {:light-type :CheapLight :state false :broken false :turnOns 0})

(defmethod turnOn :CheapLight [l]
  (if (and (not (:state l)) (< (:turnOns l) 9))
    (if (not (:broken l))
      {:light-type (:light-type l) :state true :broken (:broken l) :turnOns (+ (:turnOns l) 1)}
      l)
    {:light-type (:light-type l) :state (:state l) :broken true :turnOns (:turnOns l)}))

(defmethod turnOff :CheapLight [l]
  (if (:state l)
    {:light-type (:light-type l) :state false :broken (:broken l) :turnOns (:turnOns l)}
    l))


;;Spolecna pro oba typy svetel
(defn isDefective? [light]
  (let [defective(fn [light n]
                   (if (> n 0)
                     (recur (turnOff (turnOn light)) (dec n))
                     light)
                   )]
    (if (:broken (defective light 10))
      true
      false)))


;;testovani
(def perfectLight (light))
(def cheapLight (cheapLight))
(isDefective? cheapLight)                                   ;; => true
(isDefective? perfectLight)                                 ;; => false






















(turnOn {:light-type :light
         :turnOns 0})

(defmulti turnOn (fn [light] (:light-type light)))
(defmulti turnOff (fn [light] (:light-type light)))
(defmulti isOn? (fn [light] (:light-type light)))

(defmulti turn (fn [light turn] :light-type :state light))

(defmethod turn :light
  [light turn]
  (if (or (and (= (:turn light) "on") (= (:state light) "off"))
          (and (= (:turn light) "off") (= (:state light) "on")))
    {:light-type :light :turnOns (+ (:turnOns x) 1)}
    light))





(defmethod turnOn :light
  [x]
  (if (even? (:turnOns x))
    {:light-type :light :turnOns (+ (:turnOns x) 1)}
    x ))

(defmethod turnOff :light
  [x]
  (if (odd? (:turnOns x))
    {:light-type :light :turnOns (+ (:turnOns x) 1)}
    x ))
(defmethod is)
