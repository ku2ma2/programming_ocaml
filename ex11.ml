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


(* 問題 11.2: 次の漸化式で定義される数列 an の 第n項を求める
関数 a をデザインレシピにしたがって作れ。 *)
let rec a n = 
    if n = 0 then 3
    else 2 * a (n - 1) - 1

(* テスト *)
let a_t1 = a 0 = 3
let a_t2 = a 1 = 5
let a_t3 = a 2 = 9
let a_t4 = a 3 = 17
let a_t5 = a 4 = 33
