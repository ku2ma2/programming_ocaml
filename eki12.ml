#use "eki8.ml";;
#use "ekimei.ml";;

(* 問題 12.1: 駅名（漢字の文字列）namae、最短距離（実数） saitan_kyori、
駅名（漢字の文字列）のリスト temae_listの3つのフィールドとして持つ
レコード型 eki_t を定義せよ。 *)
type eki_t = {
    namae: string;
    saitan_kyori: float;
    temae_list: string list;
}

(* 問題 12.2: ekimei_t 型のリストを受け取ったら、その駅名を使って eki_t 型の
リストを作る関数 make_eki_list をデザインレシピにしたがって作れ。
ここで返される eki_t 型のリストには namae フィールドに（漢字の）駅名、
saitan_kyori フィールドは無限大、temae_list 
フィールドには空リストを入れておくこととする *)
(* make_eki_list : ekimei_t list -> eki_t list *)
let rec make_eki_list ekimei_lst = match ekimei_lst with
    [] -> []
  | {kanji=kanji; kana=k; romaji=r; shozoku=s} :: rest ->
      {namae=kanji; saitan_kyori=infinity; temae_list=[]} :: make_eki_list rest

(* テスト用データ *)
let eki_list1 = []
let eki_list2 = [ekimei_test1]
let eki_list3 = [ekimei_test1; ekimei_test2]

(* テスト *)
let make_eki_list_t1 = make_eki_list eki_list1 = []
let make_eki_list_t2 = make_eki_list eki_list2 = [{namae="品川"; saitan_kyori=infinity; temae_list=[]}]
let make_eki_list_t3 = make_eki_list eki_list3 = [{namae="品川"; saitan_kyori=infinity; temae_list=[]}; {namae="茗荷谷"; saitan_kyori=infinity; temae_list=[]}]


