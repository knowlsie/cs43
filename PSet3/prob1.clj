(ns pset3.prob1
  (:gen-class)
  (:require [com.climate.claypoole :as cp]))

(defn sleepy-sqrt [x]
   (Thread/sleep 2000)
   (Math/sqrt x))

(time (doall (map sleepy-sqrt (repeat 5 20)))) ;; ==> Elapsed time: 10010.30 msecs

(time (doall (pmap sleepy-sqrt (repeat 5 20)))) ;; ==> Elapsed time: 2005.594 msecs

(def pool4 (cp/threadpool 4))
(time (doall (cp/pmap pool4 sleepy-sqrt (repeat 5 20)))) ;; ==> Elapsed Time: 4002.78 msecs
(time (doall (cp/pmap pool4 sleepy-sqrt (repeat 4 20)))) ;; ==> Elapsed Time: 2001.49 msecs
(time (doall (cp/pmap pool4 sleepy-sqrt (repeat 8 20)))) ;; ==> Elapsed Time: 4004.94 msecs
(cp/shutdown pool4)

(def pool2 (cp/threadpool 2))
(time (doall (cp/pmap pool2 sleepy-sqrt (repeat 5 20)))) ;; ==> Elapsed Time: 6008.79 msecs
(time (doall (cp/pmap pool2 sleepy-sqrt (repeat 4 20)))) ;; ==> Elapsed Time: 4002.81 msecs
(time (doall (cp/pmap pool2 sleepy-sqrt (repeat 8 20)))) ;; ==> Elapsed Time: 8011.40 msecs
(cp/shutdown pool2)

(def pool8 (cp/threadpool 8))
(time (doall (cp/pmap pool8 sleepy-sqrt (repeat 5 20)))) ;; ==> Elapsed Time: 2002.64 msecs
(time (doall (cp/pmap pool8 sleepy-sqrt (repeat 8 20)))) ;; ==> Elapsed Time: 2004.83 msecs
(time (doall (cp/pmap pool8 sleepy-sqrt (repeat 16 20)))) ;; ==> Elapsed Time: 4005.353 msecs
(cp/shutdown pool8)
