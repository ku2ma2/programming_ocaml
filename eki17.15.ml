type ekimei_t = {
    kanji: string;
    kana: string;
    romaji: string;
    shozoku: string;
}

type ekikan_t = {
    kiten: string;  (* 起点の駅名 *)
    shuten: string; (* 終点の駅名 *)
    keiyu: string;  (* 経由する路線名 *)
    kyori: float;   (* ２駅間の距離（km） *)
    jikan: int;     (* 所要時間（分） *)
}

type eki_t = {
    namae: string;
    saitan_kyori: float;
    temae_list: string list;
}

;;
#use "ekimei.ml";;
#use "ekikan.ml";;
#use "eki17.14.ml";;


(* ローマ字の駅名（文字列）と駅名リスト（ekimei_t list 型）を受け取ったら、
その駅の漢字表記を文字列で返す関数
```
romaji_to_kanji "myogadani" global_ekimei_list は "茗荷谷" を返す。
```
駅名リストに目的の駅が見つからなかった場合には空文字 "" を返すようにせよ。 *)
(* romaji_to_kanji : string -> ekimei_t list -> string *)
let rec romaji_to_kanji station lst = match lst with
    [] -> ""
  | { kanji=n; kana=k; romaji=r; shozoku=s } :: rest ->
    if r = station then n
    else romaji_to_kanji station rest

(* get_ekikan_kyori は eki17.14.ml から読み込む *)


(* 直前に確定した駅 p（eki_t型）と未確定の駅リスト v（eki_t list）と
駅間の木構造 (ekikan_tree)を受け取ったら、必要な更新処理を行ったあとの
未確定の駅リストを返す関数 *)
(* koushin : eki_t -> eki_t list -> ekikan_tree_t -> eki_t list *)
let koushin p v ekikan_tree = 
    List.map (
       fun q -> match (p, q) with
           ({namae=pn; saitan_kyori=ps; temae_list=pt},
           {namae=qn; saitan_kyori=qs; temae_list=qt}) ->
               let kyori = get_ekikan_kyori pn qn ekikan_tree in
                   if kyori = infinity then q
                   else if ps +. kyori < qs then {namae=qn; saitan_kyori=ps +. kyori; temae_list= qn :: pt}
                   else q
    ) v

(* eki_t list 型のリストを受け取ったら、
「最短距離最小の駅」と「最短距離最小の駅以外からなるリスト」の組を返す関数 *)
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

(* eki_t list 型の（未確定の）駅のリストと ekikan_tree_t 型の
駅間の木構造を受け取ったら、ダイクストラのアルゴリズムにしたがって各駅について
最短距離と最短経路が正しく入ったリスト（eki_t list 型）を返す関数 *)
(* dijkstra_main : eki_t list -> ekikan_tree_t -> eki_t list *)
let rec dijkstra_main lst ekikan_tree = match lst with
    [] -> []
  | first :: rest -> 
      let (saitan, nokori) = saitan_wo_bunri (first :: rest) in
      let result = koushin saitan nokori ekikan_tree in 
      saitan :: dijkstra_main result ekikan_tree

(* 始点の駅名（ローマ字の文字列）と終点の駅名（ローマ字の文字列）を受け取ったら
- romaji_to_kanji （問題 10.10）を使って始点と終点の漢字表記（文字列）を求め
- global_ekimei_list から make_initial_eki_list（問題 14.12
  を使って駅のリスト（eki_t list型）を作り
- dijkstra_main を使って各駅までの最短路を確定し
- その中から終点の駅のレコード（eki_t型）を返す *)
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
    let global_ekikan_tree = inserts_ekikan Empty global_ekikan_list in
    let eki_list = dijkstra_main eki_list_prev global_ekikan_tree in
        find end_eki eki_list


(* テスト *)
let test1 = dijkstra "shibuya" "gokokuji" = 
  {namae = "護国寺"; saitan_kyori = 9.8; 
   temae_list = 
     ["護国寺"; "江戸川橋"; "飯田橋"; "市ヶ谷"; "麹町"; "永田町"; 
      "青山一丁目"; "表参道"; "渋谷"]} 
