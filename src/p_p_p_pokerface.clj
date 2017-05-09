(ns p-p-p-pokerface)

(def ranks {\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[rank] card]
    (ranks rank)))

(defn suit [card]
  (let [[_ s] card]
   (str s)))

(defn highest-rank-freq [hand]
  (apply max (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (= 2 (highest-rank-freq hand)))

(defn three-of-a-kind? [hand]
  (= 3 (highest-rank-freq hand)))

(defn four-of-a-kind? [hand]
  (= 4 (highest-rank-freq hand)))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
   (= [1 2 2] (sort (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [values (sort (map rank hand))
        hand-range
        (fn [values] (range (apply min values) (+ 1 (apply max values))))
        aces-low
        (fn [values] (sort (replace {14 1} values)))]
   (or
     (=
       (hand-range values)
       values)
     (=
       (hand-range (aces-low values))
       (aces-low values)))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (= 1 (count (frequencies (map suit hand))))))

(defn value [hand]
  (let [checkers #{
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        hand-value
        (fn [checker]
          (let [[check val] checker]
            (if (check hand) val 0)))]
    (apply max (map hand-value checkers))))
