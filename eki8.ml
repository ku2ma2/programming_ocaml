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
