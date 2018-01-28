(* 問題 15.3: 自然数 n 以下の素数をすべて求める簡単なアルゴリズムとして
「エラトステネスのふるい」と呼ばれる方法がある。
このアルゴリズムは以下のように動く。

(1) 2からnまでの自然数のリストを作る。
(2) リストの先頭の要素は素数である。
(3) リストの残りの中からリストの先頭の要素で割り切れるものは取り除く。
(4) (2) 以降をリストが空になるまで繰り返す。

このアルゴリズムの(2)から(4)を行う関数、つまり2以上 n以下の
自然数のリストを受け取ったら、2以上 n 以下の素数のリストを返す関数
sieve をデザインレシピにしたがって作れ。
その際、再帰の停止性についてもきちんと議論せよ。 *)
(* sieve : int list -> int list *)
let rec sieve lst = match lst with
    [] -> []
  | first :: rest -> first :: sieve (List.filter (fun x -> x mod first <> 0) rest)

(* テスト *)
let sieve_t1 = sieve [] = []
let sieve_t2 = sieve [2] = [2]
let sieve_t3 = sieve [2; 3; 4; 5; 6; 7] = [2; 3; 5; 7]

(* さらに自然数を受け取ったら、それ以下の素数のリストを返す関数
prime をデザインレシピにしたがって作れ。 *)

(* 目的: 与えらえられた n から 2 までのリストを作成する関数 *)
(* two_to_n : int -> int list *)
let two_to_n n = 
    let rec loop x = 
        if x <= n then x :: loop (x + 1)
        else [] in
    loop 2

(* テスト *)
let two_to_n_t1 = two_to_n 5 = [2; 3; 4; 5]
let two_to_n_t2 = two_to_n 2 = [2]
let two_to_n_t4 = two_to_n 6 = [2; 3; 4; 5; 6]

(* prime : int -> int list *)
let prime n = sieve ( two_to_n n )

(* テスト *)
let prime_t1 = prime 10 = [2; 3; 5; 7]
