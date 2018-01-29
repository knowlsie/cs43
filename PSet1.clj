(ns pset-1)

;; Problem 1
;; Takes: 1 or more rational numbers.
;; Returns: lowest common multiple of those numbers.
(defn lcm [initial & coll]
  (let [gcd (fn [x y]
              (loop [next-x x
                     next-y y]
                (if (= next-y 0) next-x
                  (recur next-y (mod next-x next-y)))))
        lcm_of_2 (fn [x y] (/ (* x y) (gcd x y)))]
    (reduce lcm_of_2 initial coll)))

;; Tests
(lcm 5 3 7) ;; 105
(lcm 3/4 1/6) ;; 3/2

;; Problem 2
;; Takes: No parameters
;; Returns: Lazy sequence of the Catalan numbers
(defn catalan
  ([] (catalan '(1)))
  ([s] (let [next (->> s
                      (map * (reverse s))
                      (reduce +))]
        (lazy-seq (cons (first s) (catalan (cons next s)))))))

;; Tests
(take 0 (catalan)) ;; ()
(take 5 (catalan)) ;; (1 1 2 5 14)
(take 10 (catalan)) ;; (1 1 2 5 14 32 132 429 1530 4862)

;; Problem 3

;; Takes: Vector of strings
;; Returns: Vector of cards represented as maps
;; NB: This function is maybe unnecessary in practice, but I needed
;; a bit of experience with clojure's maps "as data", and this helped!
(defn strings->cards [string-hand]
  ; This let could be some fancy zipmap stuff but this seemed simpler.
  (let [char->suits {"H" :hearts "D" :diamonds "C"
                     :clubs "S" :spades}
        char->values {"2" 2 "3" 3 "4" 4
                      "5" 5 "6" 6 "7" 7
                      "8" 8 "9" 9 "T" 10
                      "J" 11 "Q" 12 "K" 13
                      "A" 14}]
    (assert (= (count string-hand) 5)
            "Hand must contain exactly 5 cards.")
    (mapv (fn [s] {:suit (char->suits (subs s 0 1))
                   :value (char->values (subs s 1))})
          string-hand)))

;; Takes: List of integers
;; Returns: Whether those integers are consecutive
(defn consecutive-numbers? [numbers]
 (let [max-value (apply max numbers)
       min-value (apply min numbers)]
   (and (= (- max-value min-value) (- (count numbers) 1))
        (apply distinct? numbers))))

;; Takes: List of cards
;; Returns: Whether those cards are consecutive
;;          (with aces high or low)
(defn consecutive-cards? [hand]
  (let [values (map :value hand)
        mod-values (map #(mod % 13) values)]
    (or (consecutive-numbers? values)
        (consecutive-numbers? mod-values))))

;; Takes: Poker hand in strings.
;; Returns: Keyword representing the best hand available.
(defn best-poker-hand [string-hand]
  (let [hand (strings->cards string-hand)
        suit-freqs (->> hand
                        (map :suit)
                        (frequencies)
                        (vals))
        value-freqs (->> hand
                         (map :value)
                         (frequencies)
                         (vals))]
    (cond
      (and (consecutive-cards? hand)
           (some #(= 5 %) suit-freqs)) :straight-flush
      (some #(= 4 %) value-freqs) :four-of-a-kind
      (and (some #(= 3 %) value-freqs)
           (some #(= 2 %) value-freqs)) :full-house
      (some #(= 5 %) suit-freqs) :flush
      (consecutive-cards? hand) :straight
      (some #(= 3 %) value-freqs) :three-of-a-kind
      (= (sort value-freqs) [1 2 2]) :two-pair
      (some #(= 2 %) value-freqs) :pair
      :else :high-card)))

;; Tests
(best-poker-hand ["HA" "H2" "H3" "H4" "H5"]) ;; :straight-flush
(best-poker-hand ["DA" "CA" "SA" "HA" "H5"]) ;; :four-of-a-kind
(best-poker-hand ["DA" "HA" "HA" "HK" "HK"]) ;; :full-house
(best-poker-hand ["HA" "H2" "H3" "H4" "H6"]) ;; :flush
(best-poker-hand ["DA" "H2" "H3" "H4" "H5"]) ;; :straight
(best-poker-hand ["DA" "H3" "H3" "H3" "H5"]) ;; :three-of-a-kind
(best-poker-hand ["DA" "H3" "H3" "H5" "H5"]) ;; :two-pair
(best-poker-hand ["DK" "HK" "H3" "H2" "H5"]) ;; :pair
(best-poker-hand ["HA" "D2" "H3" "C9" "DJ"]) ;; :high-card


;; Problem 4
;; Takes: one or more functions (f's)
;; Returns: a new function (g), that is a composition of the old functions)
(defn my-comp [f1 & fns]
  (reduce (fn [g f]
            (fn [& params]
              (g (apply f params))))
          f1
          fns))

;; Tests
((my-comp - -) 1 2) ;; 1
((my-comp (fn [x] (mapv #(> % 1) x))
          #(cons 2 %)
          #(cons 1 %))
 '(0)) ;; [true false false]
