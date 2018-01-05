(* 問題 11.1: 0から受け取った自然数までの 2乗の和を求める関数 sum_of_square
をデザインレシピにしたがって作れ。例えば sum_of_square 4 は
0^2 + 1^2 + 2^2 + 3^2 + 4^2 = 30 を返す *)
let rec sum_of_square num = 
    if num <= 0 then 0
    else num * num + sum_of_square (num - 1)

(* テスト *)
let sum_of_square_t1 = sum_of_square 4 = 30
let sum_of_square_t2 = sum_of_square 0 = 0
let sum_of_square_t3 = sum_of_square 1 = 1
let sum_of_square_t4 = sum_of_square 2 = 5
