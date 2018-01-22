#use "eki17.10.ml";;

(* 問題 17.11: 「駅名」と「駅名と距離の組のリスト」を受け取ったら、その駅までの距離を返す関数 assoc をデザインレシピにしたがって作れ。駅がリストに見つからない場合は infinity を返すようにせよ。

例えば
assoc "後楽園" [("新大塚", 1.2); ("後楽園", 1.8)]
なら 1.8 が、また
assoc "池袋" [("新大塚", 1.2); ("後楽園", 1.8)]
なら infinity が返ってくる。 *)
(* 問題 18.3: 問題 17.11で作成した関数 assoc を、キーが見つからなかった
場合には Not_found という例外が起こるように変更せよ。
変更の前後で assoc の型はどのように変化するか *)
(* assoc : 'a -> ('a * 'b) list -> 'b *) 
let rec assoc check_eki lst = match lst with
   [] -> raise Not_found
  | (eki, distance) :: rest -> 
      if eki = check_eki then distance
      else assoc check_eki rest

(* テスト *)
let assoc_t1 = assoc "後楽園" [("新大塚", 1.2); ("後楽園", 1.8)] = 1.8
let assoc_t2 = assoc "池袋" [("新大塚", 1.2); ("後楽園", 1.8)] = infinity

