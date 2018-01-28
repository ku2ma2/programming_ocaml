#use "eki17.13.ml";;

(* 問題 17.14: （漢字の）駅名ふたつと ekikan_tree_t 型の木を
受け取ってきたら、その2駅間の距離を返す関数 get_ekikan_kyori
をデザインレシピにしたがって作れ。 *)
(* get_ekikan_kyori : string -> string -> ekikan_tree_t -> float *)
let rec get_ekikan_kyori eki1 eki2 eki_tree = match eki_tree with
    Empty -> infinity
  | Node (ln, leki, eki_lst, rn) -> 
      if eki1 < leki then get_ekikan_kyori eki1 eki2 ln
      else if eki1 > leki then get_ekikan_kyori eki1 eki2 rn
      else assoc eki2 eki_lst

(* テスト *) 
let eki_tree_t1 = inserts_ekikan Empty [ekikan1; ekikan2; ekikan3]

let get_ekikan_kyori_t1 = get_ekikan_kyori "茗荷谷" "新大塚" eki_tree_t1 = 1.2
let get_ekikan_kyori_t2 = get_ekikan_kyori "新大塚" "茗荷谷" eki_tree_t1 = 1.2
let get_ekikan_kyori_t3 = get_ekikan_kyori "もんげー" "" eki_tree_t1 = infinity

