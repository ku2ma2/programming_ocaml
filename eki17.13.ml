#use "eki8.ml";;
#use "eki17.10.ml";;
#use "eki17.12.ml";;

(* 問題 17.13: ekikan_tree_t 型の木と ekikan_t list 型の
駅間リストを受け取ったら、リストの中に含まれる駅間を全て挿入した木を返す関数
inserts_ekikan をデザインレシピにしたがって作れ *)
(* inserts_ekikan : ekikan_tree_t -> ekikan_t list -> ekikan_tree_t *)
let inserts_ekikan ekikan_tree ekikan_list = 
    List.fold_left insert_ekikan ekikan_tree ekikan_list


(* テスト *) 
let test1 = inserts_ekikan Empty [ekikan1; ekikan2; ekikan3] = 
  Node (Node (Empty, "後楽園", [("茗荷谷", 1.8)], Empty), 
	"新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)], 
        Node (Empty, 
	      "池袋", [("新大塚", 1.8)], 
	      Node (Empty, 
		    "茗荷谷", [("後楽園", 1.8); ("新大塚", 1.2)], 
		    Empty))) 
