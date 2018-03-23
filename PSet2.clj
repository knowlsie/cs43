(ns pset2.core
  (:gen-class))


(comment
  ;; ==== p1: unless ==== ;;
  ;; Write a macro 'unless', such that
  (unless true :a :b) => :b
  (unless false :a :b) => :a)




(defmacro unless
  "Reversed 'if'"
  [test & branches]
  (conj (reverse branches) test 'if))

(comment
  ;; ==== p2: LINQ inspired API ==== ;;
  ;; Write a macro 'from', such that

  (def names ["Burke", "Connor", "Frank", "Everett", "Albert", "George", "Harris", "David"])

  (from n in names
        (where (= (count n) 5))
        (orderby n)
        (select (.toUpperCase n))) => '("BURKE" "DAVID" "FRANK"))

;; This has multiple evaluation issues - sorry.
;; TODO: Fix multiple evaluation issues!
(defmacro from
  "Transforms collection with LINQ-inspired API"
  [x in coll & exprs]
  {:pre [(= in 'in) (every? #(= (count %) 2) exprs)]}
  (let
    [transforms
     (reverse
      (map (fn [expr]
             (let [[label fx] expr
                   transform (condp = label
                              'where 'filter
                              'orderby 'sort-by
                              'select 'map
                              (throw (Exception. "Invalid transform.")))]
               `(fn [c#] (~transform (fn [~x] ~fx) c#))))
           exprs))]
    `((comp ~@transforms) ~coll)))
;; ==== p3: Write a macro 'with-file': ==== ;;
;; Allows us to read the contents of a file and have the file
;; be automatically closed at the end.
(defmacro with-file [file-name & code]
  `(let [~'file (java.util.Scanner. (java.io.FileInputStream. ~file-name))]
     (try ~@code
       (catch Exception e#
         ;; For some reason this catch is never being hit...
         ;; It doesn't break it, but I'd like to know why!
         (throw e#))
       (finally
         (prn "File closed.")
         (.close ~'file)))))



;; ==== p4: macro debugging ==== ;;
;; The macro below to square the input has a bug. Can you spot it?

;; The bug was multiple evaluation of "n".
;; This is fixed:
(defmacro square [n]
  `(let [n# ~n]
     (* n# n#)))


(comment
  ;; HINT
  (let [seed (atom 9)
        next (fn [] (swap! seed inc))]
    (square (next))))

;; ==== p5: moar debugging ==== ;;
;; Given the following macro:

;; The bug was variable capture.
;; This is fixed:
(defmacro nif [val pos zero neg]
  "Numeric if. Executes pos, zero or neg depending
  if val is positive, zero or negative respectively"
  `(let [res# ~val]
     (cond (pos? res#) ~pos
           (zero? res#) ~zero
           :else ~neg)))

(comment

  ;;The fact below HOLDS true as it is:
  (nif -1
       "positive"
       "zero"
       "negative") => "negative"

  ;; However this next fact doesn't. Can you spot the bug?
  ;; Now fix it.
  (let [res (java.util.Scanner. (java.io.FileInputStream. "project.clj"))]
    (do (nif 0
             "positive"
             (prn (.nextLine res))
             (prn "negative"))
        (.close res))) => nil)
