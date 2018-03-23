(ns pset3.prob2
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.core.reducers :as r]
            [iota]
            [tesser.core :as t]
            [tesser.math :as m]))

;; Initial test

(-> (slurp "data/soi.csv")
    (str/split #"\n")
    (first)) ;; ==> "STATEFIPS, STATE..."

;; Function for lazy sequence of lines
;; NB: I chose to parameterise filename rather than a reader - cos that's what Iota does.

(defn my-line-seq
  "Takes a filename (or a scanner as second parameter) and returns a
  lazy sequence of lines in that file. TODO: Improve error handling."
  ([file-name]
   (let [scanner (java.util.Scanner. (java.io.FileInputStream. file-name))]
     (my-line-seq file-name scanner)))
  ([_ scanner]
   (if (.hasNextLine scanner)
     (lazy-seq (cons (.nextLine scanner) (my-line-seq _ scanner)))
     ())))

(take 1 (my-line-seq "data/soi.csv")) ;; ==> ("STATEFIPS, STATE ...")

; Count lines synchronously

(defn count-lines-sync [file-name]
  (->> file-name
       (my-line-seq)
       (reduce (fn [i x]
                 (inc i)) 0)))

(count-lines-sync "data/soi.csv") ;; ==> 166905
(time (count-lines-sync "data/soi.csv")) ;; Elapsed time: ~14000 msecs

;; Count lines in parallel

(defn count-lines [file-name]
  (->> file-name
       (my-line-seq)
       (vec) ; Do I need to make this a vector to make it foldable? If not, remove this line.
       (r/fold + (fn [i x] (inc i)))))

(count-lines "data/soi.csv") ;; ==> 166905
(time (count-lines "data/soi.csv")) ;; Elapsed time: ~140000 msecs

;; Count lines with ~iota~

(defn count-lines-iota [file-name]
  (->> file-name
       (iota/seq)
       (r/fold + (fn [i x] (inc i)))))

(count-lines-iota "data/soi.csv") ;; ==> 166905
(time (count-lines-iota "data/soi.csv")) ;; Elapsed time: 871.638 msecs - much faster!

(defn parse-double-string [double-string]
  "Parses string into double"
  (try (Double/parseDouble double-string)
    (catch Exception e double-string)))

(defn line->vec [line]
  "Converts one line to vector with doubles"
  (as-> line l
      (str/split l #",")
      (mapv parse-double-string l)))

(defn vec->map [header line]
  "Converts vector to map"
  ;; This function ends up doing a lot of redundant work converting to keywords every time.
  ;; Also (zipmap (mapv keyword header) line)) would have worked, but I'm dumb.
  (apply assoc {}
    (interleave (mapv keyword header)
                line)))

(defn line-vecs [file-name]
  "Converts file to foldable with vectors."
  (->> file-name
       (iota/seq)
       (rest)
       (r/map line->vec)))

(line-vecs "data/soi.csv") ;; => Lines as vectors.

(defn line-maps [file-name]
  "Converts file to foldable with maps."
  (let [lines (iota/seq file-name)
        header (line->vec (first lines))
        data (drop 1 lines)]
    (->> data
         (r/map line->vec)
         (r/map (partial vec->map header)))))

(line-maps "data/soi.csv") ;; ==> Lines as maps

(let [data-vec (into [] (line-maps "data/soi.csv"))]
  (->> (t/fuse {:covariance (m/covariance :A02300 :A00200)
                :correlation (m/correlation :A02300 :A00200)})
       (t/tesser (t/chunk 512 data-vec))))

;; ==> {:covariance 1.7444047177406094E10, :correlation 0.5643315288040673}

;; Assuming this is right - (I do have one concern about the code, but no time to check it more thoroughly) -
;; it implies a weak positive correlation between salaries/wage and unemployment compensation.
