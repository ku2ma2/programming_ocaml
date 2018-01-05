#use "eki8.ml";;
#use "ekimei.ml";;

(* 問題 10.10: ローマ字の駅名（文字列）と駅名リスト（ekimei_t list 型）を受け取ったら、
その駅の漢字表記を文字列で返す関数 romaji_to_kanji をデザインレシピにしたがって作れ。
問題9.9 で作成した global_ekimei_list を使うと、
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

(* テスト *)
let romaji_to_kanji_t1 = romaji_to_kanji "myogadani" global_ekimei_list = "茗荷谷"
let romaji_to_kanji_t2 = romaji_to_kanji "nishinippori" global_ekimei_list = "西日暮里"
let romaji_to_kanji_t3 = romaji_to_kanji "myogadanidani" global_ekimei_list = ""

;;

#use "ekikan.ml"

(* 問題 10.11: 漢字の駅名をふたつ（いずれも文字列）と駅間リスト（ekikan_t list型）
を受け取ったら、駅間リストの中からその2駅間の距離を返すを返す関数 get_ekikan_kyori
をデザインレシピにしたがって作れ。問題 9.10で作成した global_ekikan_listを使うと、例えば
```
get_ekikan_kyori "茗荷谷" "新大塚" global_ekikkan_list
```
は 1.2 を返す。2駅が直接繋がっている場合のみその距離を返し、直接繋がっていない場合は無限大
infinity を返すようにせよ。 *)

(* get_ekikan_kyori : string -> string -> ekikan_t list -> float *)
let rec get_ekikan_kyori eki1 eki2 lst = match lst with
    [] -> infinity
  | {kiten=k; shuten=s; keiyu=keiyu; kyori=kyori; jikan=j} :: rest -> 
      if (k = eki1 && s = eki2) || (s = eki1 && k = eki2) then kyori
      else get_ekikan_kyori eki1 eki2 rest

(* テスト *)
let get_ekikan_kyori_t1 = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_list = 1.2
let get_ekikan_kyori_t2 = get_ekikan_kyori "新大塚" "茗荷谷" global_ekikan_list = 1.2
let get_ekikan_kyori_t3 = get_ekikan_kyori "志茂" "妙典" global_ekikan_list = infinity
let get_ekikan_kyori_t4 = get_ekikan_kyori "もんげー" "" global_ekikan_list = infinity
let get_ekikan_kyori_t5 = get_ekikan_kyori "茗荷谷" "新大塚" [] = infinity


(* 問題 10.12: ローマ字の駅名をふたつ受けとってきたら、その間の距離を調べ、
直接つながっている場合は「A駅からB駅までは◯kmです」という文字列を返し、
つながっていない場合は「A駅とB駅はつながっていません」という文字列を返す関数
kyori_wo_hyoji をデザインレシピにしたがって作れ。文字列中の「A駅」「B駅」
には漢字を使うことにする。また、そもそも入力されたローマ字の駅名が存在
しなかった場合には「〜という駅は存在しません」という文字列を返すようにせよ。 *)
(* kyori_wo_hyoji : string -> string -> string *)
let kyori_wo_hyoji station1 station2 = 
    let eki1 = romaji_to_kanji station1 global_ekimei_list in
    let eki2 = romaji_to_kanji station2 global_ekimei_list in
        if eki1 = "" then station1 ^ "という駅は存在しません"
        else if eki2 = "" then station2 ^ "という駅は存在しません"
        else 
            let ekikan = get_ekikan_kyori eki1 eki2 global_ekikan_list in
                if ekikan = infinity then eki1 ^ "駅から" ^ eki2 ^ "駅はつながっていません"
                else eki1 ^ "駅から" ^ eki2 ^ "駅までは" ^ string_of_float ekikan ^ "kmです"

(* テスト *)
let kyori_wo_hyoji_t1 = 
    kyori_wo_hyoji "myogadani" "shinotsuka" = "茗荷谷駅から新大塚駅までは1.2kmです"
let kyori_wo_hyoji_t2 = 
    kyori_wo_hyoji "minamisenju" "ikebukuro" = "南千住駅から池袋駅はつながっていません"
let kyori_wo_hyoji_t3 = 
    kyori_wo_hyoji "mogemoge" "ebisu" = "mogemogeという駅は存在しません"
let kyori_wo_hyoji_t4 = 
    kyori_wo_hyoji "ebisu" "mogemoge" = "mogemogeという駅は存在しません"
