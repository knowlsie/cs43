(ns pset3.prob3
  (:gen-class))

;; This probably isn't an ideal data structure.
;; Sorry, I haven't taken CS 107 or 110, so I was guessing here.

(def table (ref {:philosophers
                 {:socrates  {:by [:0 :1] :holds []}
                  :descartes {:by [:1 :2] :holds []}
                  :plato     {:by [:2 :3] :holds []}
                  :aristotle {:by [:3 :4] :holds []}
                  :confucius {:by [:4 :0] :holds []}}
                 :forks
                 {:0 {:priority 0 :held false}
                  :1 {:priority 1 :held false}
                  :2 {:priority 2 :held false}
                  :3 {:priority 3 :held false}
                  :4 {:priority 4 :held false}}}))

(defn update-table [phil table]
  "Updates the table, making a particular philosopher act.
   Prints the philosopher's name each time he eats.
   Uses a priority-based system to prevent deadlock."
  (dosync
   (let [temp-table (deref table)
         held (get-in temp-table [:philosophers phil :holds])
         fork1 (get-in temp-table [:philosophers phil :by 0])
         fork2 (get-in temp-table [:philosophers phil :by 1])]
     (if
       (= (count held) 2)
       (do
         (println phil)
         (alter table assoc-in [:philosophers phil :holds] [])
         (alter table assoc-in [:forks fork1 :held] false)
         (alter table assoc-in [:forks fork2 :held] false))
       (let [priority1 (get-in temp-table [:forks fork1 :priority])
             priority2 (get-in temp-table [:forks fork2 :priority])
             smallfork (if (< priority1 priority2) fork1 fork2)
             largefork (if (< priority1 priority2) fork2 fork1)]
         (cond
           (not (get-in temp-table [:forks smallfork :held]))
           (do
             (alter table update-in [:philosophers phil :holds] conj smallfork)
             (alter table assoc-in [:forks smallfork :held] true))
           (and (= held [smallfork])
                (not (get-in temp-table [:forks largefork :held])))
           (do
             (alter table update-in [:philosophers phil :holds] conj largefork)
             (alter table assoc-in [:forks largefork :held] true))))))))

(defn run-simulation [table threads iterations]
  "Runs a simulation of a table, updating a random philosopher a given number of times
  on a given number of threads."
  (let [random-philosopher #(rand-nth (list :socrates :descartes :plato :aristotle :confucius))]
    (dorun
     (apply pcalls
            (repeat threads #(dotimes [_ iterations] (update-table (random-philosopher) table)))))))

(run-simulation table 10 500) ;; ==> Prints a lot of different philosophers eating.

(deref table) ;; ==> Valid unlocked table (hopefully)
