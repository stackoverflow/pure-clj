(ns test.clojure
  (:require [some.clojure :as c :refer [ab cd ef]])
  (:refer-clojure :exclude [apply]))

(defn myfun [x]
  (constantly x))

(def fun2
  (fn [y]
    (* y -3.123e-99)))

(defn math []
  (* 0xFD6DCA0B 0b0011101001 -1237865L))

(def ^:private pvt 9)

(def ^{:private true} pvt2 99)

(defn datas []
  (->> [1 2 3 4 5]
       (map inc)
       (filter odd?)))
