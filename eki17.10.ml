(* 問題 17.10: 木の節に

- 漢字の駅名
- 「『その駅につながっている駅名（漢字）』と『その駅までの距離』の組」

のリストを持つ木の型 ekikan_tree_t を宣言せよ。
その際、構成子としては Empty と Node とせよ。 *)
type ekikan_tree_t = 
    Empty
  | Node of ekikan_tree_t * string * (string * float) list * ekikan_tree_t


  let ekikan_tree_t_test = Node (Empty, "茗荷谷", [("新大塚", 1.2); ("後楽園", 1.8)], Empty)
