#use "ex8.ml";;

(* 問題9.1: 春、夏、秋、冬の 4つの文字列からなるリストを作れ *)
"春" :: "夏" :: "秋" :: "冬" :: [];;


(* 問題9.2: 問題8.3で定義した person_t型を要素とする長さが 3のリストを作れ *)
person_1 :: person_2 :: person_3 :: [];;


(* 問題9.3: 9.1の問題で作ったリストを要素を書き並べる方法で作れ *)
["春"; "夏"; "秋"; "冬"];;


(* 問題 9.4: 整数のリストを受け取ったら、そのリストの長さを返す関数 length 
をデザインレシピにしたがって作れ。例えば length [2; 1; 6; 4; 7]は 5を返す *)
(* 目的: 整数のリストを受け取ったら、そのリストの長さを返す関数 *)
(* length : int list -> int *)
let rec length list = match list with
    [] -> 0
  | first :: rest -> 1 + length rest

(* テスト *)
let length_test1 = length [] = 0
let length_test3 = length [6] = 1
let length_test3 = length [2; 1; 6; 4; 7] = 5

(* 問題 9.5 整数のリストを受け取ったら、その中の偶数の要素のみを含むリストを返す関数
even をデザインレシピにしたがって作れ。
例えば even [2; 1; 6; 4; 7] は [2; 6; 4] を返す *)
(* even : int list -> int list *)
let rec even list = match list with
    [] -> []
  | first :: rest -> 
        if first mod 2 = 0 then first :: even rest
        else even rest

(* テスト *)
let even_test1 = even [] = []
let even_test2 = even [2] = [2]
let even_test3 = even [2; 1; 6; 4; 7] = [2; 6; 4]

