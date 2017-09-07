(ns p-p-p-pokerface)

(def rankValues {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Character/getNumericValue rank)
      (rankValues rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn hand-ranks [hand]
  (map rank hand))

(defn hand-suits [hand]
  (map suit hand))

(defn instances [values]
  (vals (frequencies values)))

(defn most-instances [values]
  (apply max (instances values)))

(defn pair? [hand]
   (= 2 (most-instances (hand-ranks hand))))

(defn three-of-a-kind? [hand]
  (= 3 (most-instances (hand-ranks hand))))

(defn four-of-a-kind? [hand]
  (= 4 (most-instances (hand-ranks hand))))

(defn flush? [hand]
  (= 5 (most-instances (hand-suits hand))))

(defn full-house? [hand]
 (= (range 2 4) (sort (instances (hand-ranks hand)))))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand) (= [1 2 2] (sort (instances (hand-ranks hand) )))))


(defn straight2? [hand]
  (let [temp-sorted-hand (sort (hand-ranks hand))
        temp-min-rank-in-hand (apply min temp-sorted-hand)
        max-rank-in-hand (apply max temp-sorted-hand)
        sorted-hand (if (and (= 14 max-rank-in-hand) (= 2 temp-min-rank-in-hand))
                      (sort (replace {14 1} temp-sorted-hand))
                      temp-sorted-hand)
        min-rank-in-hand (if (and (= 14 max-rank-in-hand) (= 2 temp-min-rank-in-hand))
                           1
                           temp-min-rank-in-hand)]
    (= (range min-rank-in-hand (+ min-rank-in-hand 5)) sorted-hand)))

(defn straight? [hand]
  (let [value-hand (hand-ranks hand)
        ace-is-1-hand (sort (replace {14 1} value-hand))
        ace-is-14-hand (sort value-hand)
        min-rank-in-hand (apply min ace-is-14-hand)]
    (or
      (= (range 1 6) ace-is-1-hand)
      (= (range min-rank-in-hand (+ min-rank-in-hand 5)) ace-is-14-hand)) ))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)


(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        check-hand-value (fn [[func value]]
          (if (func hand)
          value
          0))]
    (apply max (map check-hand-value checkers))

    ))

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)
