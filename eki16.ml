#use "ekikan.ml";;
#use "eki15.ml";;

(* 問題 16.4: eki_t list 型の（未確定の）駅のリストと ekikan_t list 型の
駅間のリストを受け取ったら、ダイクストラのアルゴリズムにしたがって各駅について
最短距離と最短経路が正しく入ったリスト（eki_t list 型）を返す関数
dijkstra_main をデザインレシピにしたがって作れ。
その際、再帰の停止性についても議論せよ。 *)

(* dijkstra_main : eki_t list -> ekikan_t list -> eki_t list *)
let rec dijkstra_main lst ekikan_list = match lst with
    [] -> []
  | first :: rest -> 
      let (saitan, nokori) = saitan_wo_bunri (first :: rest) in
      let result = koushin saitan nokori ekikan_list in 
      saitan :: dijkstra_main result ekikan_list 

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

(* 問題 16.5: 始点の駅名（ローマ字の文字列）と
終点の駅名（ローマ字の文字列）を受け取ったら

- romaji_to_kanji （問題 10.10）を使って始点と終点の漢字表記（文字列）を求め
- global_ekimei_list から make_initial_eki_list（問題 14.12
  を使って駅のリスト（eki_t list型）を作り
- dijkstra_main を使って各駅までの最短路を確定し
- その中から終点の駅のレコード（eki_t型）を返す

ような関数 dijkstara をデザインレシピに従って作れ。 *)

let make_initial_eki_list ekimei_list kiten = 
    List.map (fun ekimei -> match ekimei with 
           {kanji = k; kana = a; romaji = r; shozoku = s} -> 
             if k = kiten 
             then {namae = k; saitan_kyori = 0.; temae_list = [k]} 
             else {namae = k; saitan_kyori = infinity; temae_list = []}) 
         ekimei_list 

(* 目的：受け取った eki_list から shuten のレコードを探し出す *) 
(* find : string -> eki_t list -> eki_t *) 
let rec find shuten eki_list = match eki_list with 
    [] -> {namae = ""; saitan_kyori = infinity; temae_list = []} 
  | ({namae = n; saitan_kyori = s; temae_list = t} as first) :: rest -> 
      if n = shuten then first else find shuten rest 

(* dijkstra : string -> string -> eki_t *)
let dijkstra shiten shuten = 
    let start_eki = romaji_to_kanji shiten global_ekimei_list in
    let end_eki = romaji_to_kanji shuten global_ekimei_list in
    let eki_list_prev = make_initial_eki_list global_ekimei_list start_eki in
    let eki_list = dijkstra_main eki_list_prev global_ekikan_list in
        find end_eki eki_list


(* テスト *)

let test1 = dijkstra "shibuya" "gokokuji" = 
  {namae = "護国寺"; saitan_kyori = 9.8; 
   temae_list = 
     ["護国寺"; "江戸川橋"; "飯田橋"; "市ヶ谷"; "麹町"; "永田町"; 
      "青山一丁目"; "表参道"; "渋谷"]} 
(* let test2 = dijkstra "myogadani" "meguro" = 
  {namae = "目黒"; saitan_kyori = 12.7000000000000028; 
   temae_list = 
     ["目黒"; "白金台"; "白金高輪"; "麻布十番"; "六本木一丁目"; "溜池山王"; 
      "永田町"; "麹町"; "市ヶ谷"; "飯田橋"; "後楽園"; "茗荷谷"]}  *)

let test2 = dijkstra "myogadani" "meguro"
 
(* 最短距離が 12.7 にならないのは、小数を２進数で表現するときの誤差のため。 
   ここではテスト結果も書いたが、これをテスト作成時に予想するのは無理なので 
   テストとして書く意味はあまりない。*) 

