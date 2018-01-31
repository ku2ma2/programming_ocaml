(* 問題 22.2: 配列を与えられたら、そこにフィボナッチ数列を順に入れた配列
を返す関数 fib_array をデザインレシピにしたがって作れ。例えば
fib_array [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|] は 
[|0; 1; 1; 2; 3; 5; 8; 13; 21; 34|] を返す。配列の大きさを
求める関数 Array.length を使って良い *)

(* fib_array : int array -> int array *)
let fib_array a = 
    let n = Array.length a in 
        if n > 0 then a.(0) <- 0; 
        if n > 1 then a.(1) <- 1; 
        for i = 2 to n - 1 do 
            a.(i) <- a.(i - 1) + a.(i - 2); 
        done; 
        a


(* テスト *)
let fib_array_t1 = fib_array [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|] = [|0; 1; 1; 2; 3; 5; 8; 13; 21; 34|]