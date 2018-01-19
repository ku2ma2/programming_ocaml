#use "eki8.ml";;
#use "eki17.10.ml";;

(* 問題 17.12: ekikan_tree_t 型の木と ekikan_t 型の駅間を受け取ったら、
その情報を挿入した木を返す関数 insert_ekikan をデザインレシピにしたがって作れ。
実際にやることは例えば

{kiten = "新大塚"; shuten = "茗荷谷"; keiyu = "丸ノ内線"; kyori = 1.2; jikan = 2}

という駅間を挿入するなら

- 新大塚が木にあるかを調べ、なければ追加する。
  その上で ("茗荷谷", 1.2)という組を新大塚の節のリストに加える。
- 茗荷谷が木にあるかを調べ、なければ追加する。
  その上で ("新大塚", 1.2)という組を茗荷谷の節のリストに加える。

のふたつである。 *)

(* 目的：受け取った kiten, shuten, kyori を ekikan_tree に挿入した木を返す *) 
(* ins_ekikan : ekikan_tree_t -> string -> string -> float -> ekikan_tree_t *)
let rec ins_ekikan ekikan_tree kiten shuten kyori = match ekikan_tree with
    Empty -> Node (Empty, kiten, [(shuten, kyori)], Empty)
  | Node (ln, eki, connect, rn) -> 
      if kiten < eki
          then Node ((ins_ekikan ln kiten shuten kyori), eki, connect, rn)
      else if kiten > eki
          then Node (ln, eki, connect, (ins_ekikan rn kiten shuten kyori))
      else Node (ln, eki, (shuten, kyori) :: connect, rn)

    
(* 駅間の例 *) 
let ekikan1= 
    {kiten="池袋"; shuten="新大塚"; keiyu="丸ノ内線"; kyori=1.8; jikan=3}  
let ekikan2 = 
    {kiten="新大塚"; shuten="茗荷谷"; keiyu="丸ノ内線"; kyori=1.2; jikan=2} 
let ekikan3 = 
    {kiten="茗荷谷"; shuten="後楽園"; keiyu="丸ノ内線"; kyori=1.8; jikan=2} 

(* insert_ekikan_single テスト *) 
let ins_test1 = ins_ekikan Empty "新大塚" "池袋" 1.8 = 
  Node (Empty, "新大塚", [("池袋", 1.8)], Empty) 

(* ins_ekikan を始点と終点を入れ替えて2回呼ぶことでリストを作っている *)
(* insert_ekikan : ekikan_tree_t -> ekikan_t -> ekikan_tree_t *)
let insert_ekikan ekikan_tree ekikan = match ekikan with 
    {kiten=k; shuten=s; keiyu=kei; kyori=d; jikan=j} ->
        ins_ekikan (ins_ekikan ekikan_tree s k d) k s d 

   
(* テスト *) 
let tree1 = insert_ekikan Empty ekikan1 
let test1 = tree1 = 
  Node (Empty, "新大塚", [("池袋", 1.8)], 
    Node (Empty, "池袋", [("新大塚", 1.8)], Empty)) 
let tree2 = insert_ekikan tree1 ekikan2 
let test2 = tree2 = 
  Node (Empty, "新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)], 
    Node (Empty, "池袋", [("新大塚", 1.8)], 
          Node (Empty, "茗荷谷", [("新大塚", 1.2)], Empty))) 
let tree3 = insert_ekikan tree2 ekikan3 
let test3 = tree3 = 
  Node (Node (Empty, "後楽園", [("茗荷谷", 1.8)], Empty), 
    "新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)], 
        Node (Empty, 
          "池袋", [("新大塚", 1.8)], 
          Node (Empty, 
            "茗荷谷", [("後楽園", 1.8); ("新大塚", 1.2)], 
            Empty))) 

