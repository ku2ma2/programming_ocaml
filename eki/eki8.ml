(* 問題 8.5: 駅名の情報を格納するレコード型 ekimei_t を宣言せよ。
ekimei_t 型にはフィールドとして、漢字の駅名（kanji）、ひらがなの駅名（kana）、
ローマ字の駅名（romaji）および、その駅が所属する路線名（shozoku）の4つ
（いずれも文字列）を持つようにせよ。 *)

type ekimei_t = {
    kanji: string;
    kana: string;
    romaji: string;
    shozoku: string;
}

let ekimei_test1 = {kanji="品川"; kana="しながわ"; romaji="shinagawa"; shozoku="京浜東北線"}
let ekimei_test2 = {kanji="茗荷谷"; kana="みょうがだに"; romaji="myogadani"; shozoku="丸ノ内線"}

(* 問題 8.6: ekimei_t 型のデータを受け取ってきたら、「路線名、駅名（かな）」
の形式の文字列を返す関数 hyoji をデザインレシピにしたがって作れ。 *)
(* 目的: ekimei_t 型のデータを受け取ってきたら、「路線名、駅名（かな）」の形式
の文字列を返す関数 *)
(* hyoji : ekimei_t -> string *)
let hyoji ekimei_t = match ekimei_t with
    {kanji=k; kana=kn; romaji=r; shozoku=s} -> s ^ "、" ^ k ^ "（" ^ kn ^ "）"

(* テスト *)
let hyoji_t1 = hyoji ekimei_test1 = "京浜東北線、品川（しながわ）"
let hyoji_t2 = hyoji ekimei_test2 = "丸ノ内線、茗荷谷（みょうがだに）"

(* 問題 8.7: 駅と駅の間の接続情報を格納するレコード型 ekikan_t を宣言せよ。
ekikan_t 型にはフィールドとして、起点の駅名 kiten、終点の駅名 shuten、
経由する路線名 keiyu （いずれも漢字の文字列）、その2駅間の距離 kyori（km, 実数）、
そして所要時間 jikan（分, 整数）を持つようにせよ。 *)

type ekikan_t = {
    kiten: string;  (* 起点の駅名 *)
    shuten: string; (* 終点の駅名 *)
    keiyu: string;  (* 経由する路線名 *)
    kyori: float;   (* ２駅間の距離（km） *)
    jikan: int;     (* 所要時間（分） *)
}


(* 問題 12.1: 駅名（漢字の文字列）namae、最短距離（実数） saitan_kyori、
駅名（漢字の文字列）のリスト temae_listの3つのフィールドとして持つ
レコード型 eki_t を定義せよ。 *)
type eki_t = {
    namae: string;
    saitan_kyori: float;
    temae_list: string list;
}
