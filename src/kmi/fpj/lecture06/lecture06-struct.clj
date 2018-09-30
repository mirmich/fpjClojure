(defprotocol Vehicle
  (turn  [v angle])
  (drive [v distance])
  (desc  [v]))

(defrecord Car      [x y direction])
(defrecord Airplane [x y direction])
(defrecord Taxi     [x y direction revenue])

(def vehicle-impl
  {:turn  (fn [v dir] (assoc v :direction dir))
   :drive (fn [v distance]
            (let [angle (Math/toRadians (:direction v))]
              (assoc v :x (+ (:x v) (* (Math/cos angle) distance))
                       :y (+ (:y v) (* (Math/sin angle) distance)))))})


(extend Car Vehicle
  (assoc vehicle-impl
    :desc (fn [v]
            (str "Jsem auto na souradnicich " (:x v) ", " (:y v)))))

(extend Airplane Vehicle
  (assoc vehicle-impl
    :desc (fn [v]
            (str "Jsem letadlo na souradnicich " (:x v) ", " (:y v)))))

(def FARE 30)
(extend Taxi Vehicle
  (assoc vehicle-impl
    :drive (fn [v distance]
             (assoc ((:drive vehicle-impl) v distance)
               :revenue (+ (:revenue v) (* FARE distance))))
    :desc (fn [v]
            (str "Jsem taxi na souradnicich " (:x v) ", " (:y v)))))


(desc (drive (turn (drive (->Car 0 0 0) 20) 90) 20))
(desc (drive (turn (drive (->Airplane 0 0 0) 20) 90) 20))
(-> (->Airplane 0 0 0) (drive 10) (turn 45) (drive 10) (desc))
(-> (->Taxi 0 0 0 0) (drive 10) (turn 20) (drive 5) :revenue)


