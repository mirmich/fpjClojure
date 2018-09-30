(ns kmi.fpj.lecture05.lecture05)
(defprotocol Switchable
  "Protocol describing Switchable"
  (turnOn   [x] "Turns on")
  (turnOff  [x] "Turns off")
  (isOn?    [x] "Is turned on?"))

(deftype CheapLight [turnOns]
  Switchable
  (turnOn [this] (if (< turnOns 20)
                   (if (even? turnOns)
                     (set! (.-turnOns this) (+ (.-turnOns this) 1))
                     (println "Light is already turned on"))
                   (println "Light is Broken")))
  (turnOff [this] (if (< turnOns 20)
                    (if (odd? turnOns)
                      (set! (.-turnOns this) (+ (.-turnOns this) 1))
                      (println "Light is already turned off"))
                    (println "Light is Broken")))
  (isOn? [this] (odd? turnOns)))

(deftype Light [turnOns]
  Switchable
  (turnOn [this]
    (if (even? turnOns)
      (set! (.-turnOns this) (+ (.-turnOns this) 1))
      (println "Light is already turned on")))
  (turnOff [this]
    (if (odd? turnOns)
      (set! (.-turnOns this) (+ (.-turnOns this) 1))
      (println "Light is already turned off")))
  (isOn? [this] (odd? turnOns)))

(defn isDefective? [light]
  (do (dotimes [n 20] (do (turnOn light) (turnOff light)))
      (if (turnOn light)
        false
        true)))

(def light (->Light 0))
(def clight (->CheapLight 0))
(isDefective? light)
(isDefective? clight)
