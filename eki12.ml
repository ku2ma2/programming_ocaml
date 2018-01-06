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

(* 問題 12.3: 上で作ったリストのうち始点のみについては
「saitan_kyoriは0.」
「temae_listは始点の駅名のみからなるリスト」にしたい。
eki_t 型のリストと起点（漢字の文字列）を受け取ったら、起点のみ上記のようになっている
eki_t 型のリストを返す関数 shokika をデザインレシピにしたがって作れ。 *)
(* shokika : eki_t list -> string -> eki_t list *)
let rec shokika lst station = match lst with
    [] -> []
  | ({namae=n; saitan_kyori=s; temae_list=k} as first) :: rest ->
      let shokika_rest = shokika rest station in
          if n = station then {namae=n; saitan_kyori=0.; temae_list=[n]} :: shokika_rest
          else first :: shokika_rest

(* テスト用データ *)
let shokika_eki_t1 = make_eki_list eki_list1
let shokika_eki_t2 = make_eki_list eki_list2
let shokika_eki_t3 = make_eki_list eki_list3

(* テスト *)
let shokika_t1 = shokika shokika_eki_t1 "品川" = []
let shokika_t2 = shokika shokika_eki_t2 "品川" = [{namae="品川"; saitan_kyori=0.; temae_list=["品川"]}]
let shokika_t3 = shokika shokika_eki_t3 "茗荷谷" = [{namae="品川"; saitan_kyori=infinity; temae_list=[]}; {namae="茗荷谷"; saitan_kyori=0.; temae_list=["茗荷谷"]}]

(* 問題 12.4: ekimei_t 型のリストを受け取ったら、それをひらがなの順に整列し
さらに駅の重複を除いた ekimei_t 型リストを返す seiretsu をデザインレシピにしたがって作れ。
問題 10.2で作ったins＿sortを参考にして構わない *)

(* ekimei_ins : ekimei_t list -> ekimei_t -> ekimei_t list *)
let rec ekimei_ins lst eki = match lst with
    [] -> eki :: []
  | ({kanji=k; kana=kana; romaji=r; shozoku=s} as first) :: rest -> 
      match eki with
        {kanji=k0; kana=ins_kana; romaji=r0; shozoku=s0} ->
            if kana = ins_kana then ekimei_ins rest eki
            else if kana < ins_kana then first :: ekimei_ins rest eki
            else eki :: lst

  
(* テスト *)
let ekimei_ins_data1 = {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}
let ekimei_ins_data2 = {kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"}
let ekimei_ins_data3 = {kanji="表参道"; kana="おもてさんどう"; romaji="omotesandou"; shozoku="千代田線"}

let ekimei_ins_t1 = ekimei_ins [] ekimei_ins_data1 = [ekimei_ins_data1]
let ekimei_ins_t2 = ekimei_ins [ekimei_ins_data1] ekimei_ins_data2 = [ekimei_ins_data2; ekimei_ins_data1]
let ekimei_ins_t3 = ekimei_ins [ekimei_ins_data2] ekimei_ins_data1 = [ekimei_ins_data2; ekimei_ins_data1]
let ekimei_ins_t4 = ekimei_ins [ekimei_ins_data3; ekimei_ins_data1] ekimei_ins_data2 = [ekimei_ins_data3; ekimei_ins_data2; ekimei_ins_data1]

(* seiretsu : ekimei_t list -> ekimei_t list *)
let rec seiretsu lst = match lst with
    [] -> []
  | first :: rest -> ekimei_ins (seiretsu rest) first

(* テスト用データ *)
let seiretsu_data_t1 = []
let seiretsu_data_t2 = [{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}]
let seiretsu_data_t3 = [
    {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
    {kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"}; 
    {kanji="表参道"; kana="おもてさんどう"; romaji="omotesandou"; shozoku="千代田線"}; 
]
(* テスト *)
let seiretsu_t1 = seiretsu seiretsu_data_t1 = []
let seiretsu_t2 = seiretsu seiretsu_data_t2 = [{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}]
let seiretsu_t3 = seiretsu seiretsu_data_t3 = [
    {kanji="表参道"; kana="おもてさんどう"; romaji="omotesandou"; shozoku="千代田線"}; 
    {kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"}; 
    {kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
]

