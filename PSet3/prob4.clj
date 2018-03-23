(ns pset3.prob4
  (:gen-class)
  (:require [clojure.core.async
             :as async
             :refer [>! <! >!! <!! go chan buffer close! thread alts! alts!! timeout]]))
;; ------
;; Part 1
;; ------

(defn buggy-sleepy-sqrt [x cb]
  "Deliberately buggy node-style function to test with."
  (let [answer (Math/sqrt x)]
    (Thread/sleep 500)
    (if (< answer 1)
      (cb (Exception. "I'm not very good at small numbers, my b..."))
      (cb nil answer))))

;; Example usage in Node style.

(buggy-sleepy-sqrt
 5 (fn [err & args] (if err (throw err) args))) ;; ==> 2.23...

(buggy-sleepy-sqrt
 0.9 (fn [err & args] (if err (throw err) args))) ;; ==> Exception.

(defn run-task [f & args]
  (let [result-chan (chan)]
    (go
     (apply f (conj (vec args)
                    (fn [err & results]
                      (go
                        (if err (throw err) ; I really wasn't sure how to handle errors properly, sorry.
                          ;; (>! result-chan err)
                          (>! result-chan results)))))))
    result-chan))

;; Usage

(let [result-chan (run-task buggy-sleepy-sqrt 3)]
  (go
    (try
      (println (<! result-chan))
      (catch Exception e
        (throw e))))
  (println "Running!")) ;; ==> Prints (1.732...)

;; -----------------------------------------
;; Part 2: Not sure this is correct boogaloo
;; -----------------------------------------

(defn execute [context & fs]
  (reduce (fn [c f] (go (f (<! c))))
          (go context)
          fs))

(<!! (execute 8 inc (fn [x] (* x x)) inc)) ;; ==> 82

;; NB: Below this point is broken code I wanted to keep.
;; It doesn't work for async like at all, but it's kinda fun.

(defn execute-n [context & fs]
  "Makes a pipeline of channels, and threads the context through those channels."
  (let [chans (repeatedly (+ (count fs) 1) #(chan))
        chan-pairs (clojure.core/partition 2 1 chans)
        pipeline (map-indexed
                  (fn [i cp] {:f (nth fs i) :c1 (first cp) :c2 (second cp)})
                  chan-pairs)]
    (go (>! (first chans) context))
    (mapv
     (fn [m] (go (>! (:c2 m) ((:f m) (<! (:c1 m))))))
     pipeline)
    (last chans)))

(<!! (execute-n 1)) ;; ==> 1
(<!! (execute-n 8 inc (fn [x] (* x x)) inc)) ;; ==> 82, so it at least works for synchronous functions...
