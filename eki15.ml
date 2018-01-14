#use "eki13.ml";;

(* 問題 15.4: eki_t list 型のリストを受け取ったら、
「最短距離最小の駅」と「最短距離最小の駅以外からなるリスト」の組
を返す関数 saitan_wo_bunki をデザインレシピにしたがって作れ *)


(* saitan_wo_bunri : eki_t list -> eki_t * eki_t list *)
let saitan_wo_bunri lst = 
    List.fold_right 
        (
            fun first (p, v) ->
                match (first, p) with 
                    ( {namae=fn; saitan_kyori=fs; temae_list=ft},
                      {namae=pn; saitan_kyori=ps; temae_list=pt} ) ->
                      (* 初期値だったら最小の駅を差替えてリストにはなにもしない *)
                      if pn = "" then (first, v) 
                      (* 最短距離が小さい駅だった場合は、最小の駅の差替えとリストへの追加 *)
                      else if fs < ps then (first, p :: v)
                      (* それ以外はvリストへの追加のみにする p はそのまま p *)
                      else (p, first :: v) 
        )
        lst (* 適用するリスト *)
        ({namae=""; saitan_kyori = infinity; temae_list = []}, []) (* 初期値 *)



(* 駅の例 *) 
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} 
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} 
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} 
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} 
 
(* 駅リストの例 *) 
let lst = [eki1; eki2; eki3; eki4] 

(* テスト *) 
let saitan_wo_bunki_t1 = saitan_wo_bunri lst = (eki3, [eki1; eki2; eki4]) 
(* let saitan_wo_bunki_t1 = saitan_wo_bunri lst *)

