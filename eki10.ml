#use "eki8.ml" 
#use "ekimei.ml"

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
