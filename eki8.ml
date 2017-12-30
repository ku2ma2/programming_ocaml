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
