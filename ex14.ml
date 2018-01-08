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