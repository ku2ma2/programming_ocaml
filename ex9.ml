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


(* 問題 9.6: 文字列のリストを受け取ったら、その中の要素を前から順に全部、くっつけた文字列
を返す関数 concat をデザインレシピにしたがって作れ。
例えば concat ["春"; "夏"; "秋"; "冬"] は "春夏秋冬" を返す。 *)
(* concat : string list -> string *)
let rec concat lst = match lst with
    [] -> ""
  | first :: rest -> first ^ concat rest

(* テスト *)   
let concat_test1 = concat [] = ""
let concat_test2 = concat ["春"] = "春"
let concat_test3 = concat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬"


(* count_ketsueki_A : person_t list -> int *)
let rec count_ketsueki_A person = match person with
    [] -> 0
  | {name=n; height=h; weight=w; birthday=b; blood=bl} :: rest ->
      if bl = "A型" then 1 + count_ketsueki_A rest
      else count_ketsueki_A rest

(* テストデータ *)
let persons_1 = [person_3]
let persons_2 = [person_1; person_2; person_3; person_4]

(* テスト *)
let count_ketsueki_A_test1 = count_ketsueki_A [] = 0
let count_ketsueki_A_test2 = count_ketsueki_A persons_1 = 1
let count_ketsueki_A_test3 = count_ketsueki_A persons_2 = 2


