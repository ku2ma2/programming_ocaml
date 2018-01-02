(* 問題 10.1: あらかじめ昇順に並んでいる整数のリスト lst と整数 n を受け取ったら、
lst　を前から順に見ていき、昇順となる位置に n を挿入したリストを返す関数 insert
をデザインレシピにしたがって作れ、例えば insert [1; 3; 4; 7; 8] 5 は 
[1; 3; 4; 5; 7; 8] を返す  *)
(* insert : int list -> int -> int list *)
let rec insert lst n = match lst with
    [] -> n :: []
  | first :: rest -> 
      if first < n then first :: insert rest n
      else n :: lst

(* テスト *)
let insert_test1 = insert [] 1 = [1]
let insert_test2 = insert [1] 3 = [1; 3]
let insert_test3 = insert [3] 1 = [1; 3]
let insert_test4 = insert [1; 3; 4; 7; 8] 5 = [1; 3; 4; 5; 7; 8]

(* 問題 10.2: 整数のリストを受け取ったら、それを昇順に整列したリストを返す関数
ins_sort をデザインレシピに従って作れ 例えば ins_sort [5; 3; 8; 1; 7; 4] は
[1; 3; 4; 5; 7; 8] を返す。問題 10.1で作った insert 使用して構わない。 *)
(* ins_sort : int list -> int list *)
let rec ins_sort lst = match lst with
    [] -> []
  | first :: rest -> insert (ins_sort rest) first

(* テスト *)
let ins_sort_test1 = ins_sort [] = []
let ins_sort_test2 = ins_sort [3] = [3]
let ins_sort_test2 = ins_sort [3; 1] = [1; 3]
let ins_sort_test2 = ins_sort [5; 3; 8; 1; 7; 4] = [1; 3; 4; 5; 7; 8]



