(ns torihiki-chart.candle
  "取引テープ → OHLCV。**足はブロックであって時間ではない。**

  ## なぜブロック足なのか

  `torihiki` は決定性のために wall clock を持たない —— エンジンの README が
  『No wall clock. Logical time arrives in the block header』と書いているとおり、
  時刻はブロックヘッダから来るのであって `System/currentTimeMillis` からは
  来ない。node が返す tape の要素は `{:level :qty :side :h}` で、**`:h`
  (block height) が唯一の時間軸**。

  したがってバケットは N ブロック単位になる。これを『時間足に見えるように』
  時刻へ変換して描くことはしない —— 変換に使える真実がどこにも無く、作れば
  嘘になる。**1 ブロックの実時間は一定ではない**（validator は 200ms tick を
  目標に走るが、view change や Durable Object の eviction で伸びる）ので、
  等間隔のブロック足を等間隔の時間として読むのは誤読で、軸ラベルはそれが
  時刻でないことを見た目で示す責任を負う（`torihiki-chart.axis` 参照）。

  ## なぜ整数のままなのか

  価格は tick、数量は lot の整数で、`torihiki` は i53 の整数演算で閉じている
  （`torihiki.fixed`）。ここでも **float を一切使わない**。

  理由は表示の綺麗さではなく **同一性**。tape は誰でも読めるので、二つの
  クライアントが同じ tape から同じ足を出せることには意味がある —— チャートが
  食い違えば、どちらかが嘘をついている。集計に float が入ると丸めの差でそれが
  崩れる。float に落ちるのは `d3` の scale に渡す最後の一段だけで、そこは画素
  座標なので、差が出ても値ではなく描画の 1px に留まる。

  volume も同じ理由で lot の総和のまま持つ。ドル換算は表示の都合であって
  集計の都合ではない。"
  (:require [clojure.string]))

(defn bucket
  "`h` が属するバケットの開始 height。`span` ブロックごとに区切る。

  `0` 起点で切るので、同じ `span` を渡した二者は必ず同じ境界を得る —— tape の
  先頭を起点にすると、いつ読んだかで境界がずれて足が一致しなくなる。"
  [span h]
  (* span (quot h span)))

(defn- fold-fill
  "1 件の fill を足に畳む。tape は**古い順**に渡されている前提
  （`close` は最後に見た値、`open` は最初に見た値）。"
  [c {:keys [level qty side]}]
  (if (nil? c)
    {:open level :high level :low level :close level
     :volume qty
     :buy-volume (if (= side :buy) qty 0)
     :sell-volume (if (= side :buy) 0 qty)
     :fills 1}
    (-> c
        (assoc :close level)
        (update :high max level)
        (update :low min level)
        (update :volume + qty)
        (update (if (= side :buy) :buy-volume :sell-volume) + qty)
        (update :fills inc))))

(defn- normalize-side
  "tape の `:side` は JSON 経由で文字列にも keyword にもなりうる。**片方だけ
  扱うと上下の色分けが黙って全部 sell 側に倒れる**（比較が false を返すだけで
  エラーにならない）ので、ここで一度だけ正規化する。"
  [s]
  (if (or (= s :buy) (= s "buy")) :buy :sell))

(defn candles
  "tape → ブロック足のベクタ（height 昇順）。

  `tape` は `{:level :qty :side :h}` の seq。順序は問わない —— height で
  ソートしてから畳むので、node が新しい順で返しても正しい足になる
  （`/trades` は実際に新しい順で返す）。

  返す各足:

      {:h      バケット開始 height
       :open :high :low :close   tick 整数
       :volume :buy-volume :sell-volume   lot 整数
       :fills  この足に入った約定数}

  **空のバケットは返さない。** 約定が無いブロック区間に『前の終値で横ばいの
  足』を作るのは、起きていないことを描くことになる。x 軸を等間隔にするのは
  `torihiki-chart.axis` の仕事で、そこは欠けを欠けとして扱う。"
  [span tape]
  (->> tape
       (map (fn [f] (update f :side normalize-side)))
       (sort-by :h)
       (reduce (fn [acc f]
                 (let [b (bucket span (:h f))]
                   (update acc b fold-fill f)))
               (sorted-map))
       (mapv (fn [[h c]] (assoc c :h h)))))

(defn direction
  "足の向き。`:up` / `:down` / `:flat`。

  終値と始値の比較だけで決める。taker side の多数決ではない —— 1 ブロックには
  両側の fill が入るので『この足は買い』は近似にしかならず、近似を色にすると
  『赤い足なのに値が上がっている』が起きる。`:buy-volume` / `:sell-volume` は
  別の意味（誰が板を叩いたか）として残してあり、必要なら出来高の内訳として
  描けばよい。"
  [{:keys [open close]}]
  (cond (> close open) :up
        (< close open) :down
        :else :flat))

(defn extent
  "足の集合の価格レンジ `[low high]`（tick 整数）。足が無ければ `nil`。

  高値と安値の両方を見る —— 終値だけで取ると、ヒゲが枠の外に出る。"
  [cs]
  (when (seq cs)
    [(reduce min (map :low cs))
     (reduce max (map :high cs))]))

(defn volume-max
  "出来高軸の上限（lot 整数）。足が無ければ `nil`。"
  [cs]
  (when (seq cs)
    (reduce max (map :volume cs))))
