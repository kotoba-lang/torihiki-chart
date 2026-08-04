(ns torihiki-chart.candle-test
  (:require [clojure.test :refer [deftest is testing]]
            [torihiki-chart.candle :as c]))

(def tape
  "node の `/trades` の形。**新しい順**で返ってくることに注意（実測）。"
  [{:level 68030 :qty 2 :side "sell" :h 27}
   {:level 68020 :qty 5 :side "buy"  :h 25}
   {:level 67990 :qty 1 :side "sell" :h 24}
   {:level 68010 :qty 3 :side "buy"  :h 22}
   {:level 68000 :qty 4 :side "buy"  :h 21}])

(deftest buckets-are-absolute-not-relative-to-the-tape
  ;; tape の先頭を起点に切ると、いつ読んだかで境界がずれて足が一致しなくなる。
  (is (= 20 (c/bucket 10 21)))
  (is (= 20 (c/bucket 10 29)))
  (is (= 30 (c/bucket 10 30)))
  (is (= 0 (c/bucket 10 0))))

(deftest tape-order-does-not-change-the-candles
  ;; node は新しい順で返すが、足は古い順に畳まないと open/close が入れ替わる。
  (is (= (c/candles 10 tape)
         (c/candles 10 (reverse tape))
         (c/candles 10 (shuffle tape)))))

(deftest one-span-one-candle
  ;; span 10 なら h21..h27 は全部バケット 20 に入る。足が 2 本出ると思って
  ;; 書いて落ちた —— バケットは絶対座標なので、tape の広がりではなく span で
  ;; 決まる。
  (let [cs (c/candles 10 tape)]
    (is (= 1 (count cs)))
    (is (= 20 (:h (first cs))))))

(deftest ohlc-is-folded-in-height-order
  (let [[a b] (c/candles 5 tape)]     ; -> バケット 20 (h21,22,24) と 25 (h25,27)
    (is (= [20 25] [(:h a) (:h b)]))
    (is (= 68000 (:open a)) "最も古い height (h21) の値")
    (is (= 67990 (:close a)) "最も新しい height (h24) の値")
    (is (= 68010 (:high a)))
    (is (= 67990 (:low a)))
    (is (= 8 (:volume a)) "4+3+1")
    (is (= 3 (:fills a)))
    (is (= {:buy 7 :sell 1}
           {:buy (:buy-volume a) :sell (:sell-volume a)}))
    (is (= [68020 68030] [(:open b) (:close b)]))
    (is (= 7 (:volume b)) "5+2")))

(deftest side-is-normalised-so-the-colouring-does-not-silently-collapse
  ;; JSON 経由で文字列、EDN 経由で keyword。片方だけ扱うと比較が false を
  ;; 返すだけで、buy が全部 sell に倒れる（エラーにならない）。
  (let [[k] (c/candles 100 [{:level 1 :qty 7 :side :buy :h 1}])
        [s] (c/candles 100 [{:level 1 :qty 7 :side "buy" :h 1}])]
    (is (= 7 (:buy-volume k) (:buy-volume s)))
    (is (= 0 (:sell-volume k) (:sell-volume s)))))

(deftest empty-buckets-are-absent-not-invented
  ;; 約定の無い区間に横ばいの足を作るのは、起きていないことを描くこと。
  (let [cs (c/candles 1 [{:level 5 :qty 1 :side "buy" :h 1}
                         {:level 6 :qty 1 :side "buy" :h 9}])]
    (is (= [1 9] (mapv :h cs)))
    (is (= 2 (count cs)))))

(deftest direction-follows-open-to-close-not-taker-side
  ;; 1 ブロックに両側の fill が入るので taker side の多数決は近似にしかならず、
  ;; 近似を色にすると『赤い足なのに値が上がっている』が起きる。
  (let [[c1] (c/candles 100 [{:level 10 :qty 9 :side "sell" :h 1}
                             {:level 20 :qty 1 :side "buy" :h 2}])]
    (is (= :up (c/direction c1)) "sell 出来高が 9 倍でも終値が上なら上げ")
    (is (< (:buy-volume c1) (:sell-volume c1)) "内訳は内訳として残っている"))
  (is (= :flat (c/direction {:open 5 :close 5})))
  (is (= :down (c/direction {:open 5 :close 4}))))

(deftest extent-uses-wicks-not-bodies
  ;; 終値だけで枠を取ると、ヒゲが枠の外に出る。
  (let [cs (c/candles 100 [{:level 100 :qty 1 :side "buy" :h 1}
                           {:level 300 :qty 1 :side "buy" :h 2}
                           {:level 200 :qty 1 :side "buy" :h 3}])]
    (is (= [100 300] (c/extent cs)))
    (is (= 3 (c/volume-max cs)) "1 本しか無いので総和")))

(deftest empty-input-is-nil-not-a-zero-range
  (is (nil? (c/extent [])))
  (is (nil? (c/volume-max [])))
  (is (= [] (c/candles 10 []))))

(deftest everything-stays-integral
  (testing "float が混じると同じ tape から違う足が出る"
    (doseq [c (c/candles 10 tape)
            [_ v] (select-keys c [:open :high :low :close :volume
                                  :buy-volume :sell-volume :fills :h])]
      (is (integer? v) (str "整数でない値: " v)))))
