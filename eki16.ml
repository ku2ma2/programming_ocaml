#use "eki13.ml";;
#use "eki15.ml";;

(* 問題 16.4: eki_t list 型の（未確定の）駅のリストと ekikan_t list 型の
駅間のリストを受け取ったら、ダイクストラのアルゴリズムにしたがって各駅について
最短距離と最短経路が正しく入ったリスト（eki_t list 型）を返す関数
dijkstra_main をデザインレシピにしたがって作れ。
その際、再帰の停止性についても議論せよ。 *)

(* dijkstra_main : eki_t list -> ekikan_t list -> eki_t list *)
let dijkstra_main lst ekikan_list = []

(* 駅の例 *) 
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} 
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} 
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} 
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} 
 
(* 駅リストの例 *) 
let dijkstra_main_lst = [eki1; eki2; eki3; eki4] 
 
(* テスト *) 
let dijkstra_main_t1 = dijkstra_main [] global_ekikan_list = [] 
let dijkstra_main_t2 = dijkstra_main dijkstra_main_lst global_ekikan_list = 
  [{namae = "茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]}; 
   {namae = "新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]}; 
   {namae = "後楽園"; saitan_kyori = 1.8; temae_list = ["後楽園"; "茗荷谷"]}; 
   {namae = "池袋"; saitan_kyori = 3.; temae_list = ["池袋"; "新大塚"; "茗荷谷"]}] 

