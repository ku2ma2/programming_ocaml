(* 問題 14.1: 問題 9.5で作成した関数 even を filter 
を用いて定義せよ。 *)

(* 目的: 整数のリストを受け取ったら、その中の偶数の要素のみを含むリストを返す関数
例えば even [2; 1; 6; 4; 7] は [2; 6; 4] を返す *)
(* even : int list -> int list *)
let rec even lst = 
    let f x = x mod 2 = 0 in
    List.filter f lst

(* テスト *)
let even_test1 = even [] = []
let even_test2 = even [2] = [2]
let even_test3 = even [2; 1; 6; 4; 7] = [2; 6; 4]


(* 問題 14.3: 問題 9.6 で作成した関数 concat を fold_right を使って書き直せ *)

(* 目的: init から始めて lst の要素を右から順に f を施しこむ *)
(* fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
let rec fold_right f lst init = match lst with
    [] -> init
  | first :: rest -> f first (fold_right f rest init)

(* 目的: 文字列を後方の結果に連結する関数 *)
let append_str first rest_result = first ^ rest_result

(* concat : string list -> string *)
let concat lst = fold_right append_str lst ""

(* テスト *)   
let concat_test1 = concat [] = ""
let concat_test2 = concat ["春"] = "春"
let concat_test3 = concat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬"

