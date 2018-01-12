(* 問題 16.1: 整数のリストを受け取ったら、それまでの数の合計からなる
リストを返す関数 sum_list をデザインレシピニしたがって作れ。
例えば sum_list [3; 2; 1; 4] は [3; 5; 6; 10] を返す *)
(* sum_list : int list -> int list *)
let sum_list lst = 

    (* アキュムレータ: totalがこれまでの合計  *)
    (* sum_sub : int list -> int -> int list *)
    let rec sum_sub sum_list total = match sum_list with
        [] -> []
      | first :: rest -> (total + first) :: sum_sub rest (total + first)

    in sum_sub lst 0

(* テスト *)
let sum_list_t1 = sum_list [] = []
let sum_list_t2 = sum_list [3; 2; 1; 4] = [3; 5; 6; 10]
let sum_list_t3 = sum_list [3] = [3]
let sum_list_t4 = sum_list [3; 3] = [3; 6]

