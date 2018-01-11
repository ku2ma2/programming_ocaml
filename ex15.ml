(* 問題 15.1: ここで作った関数 quick_sort は、実は入力によって
は正しい答えを返さない場合がある。それはどのような場合か。
またプログラムをどのように変更すれば正しい答えを返すようになるか。 *)
(* 目的: 受け取った lst をクイックソートを使って昇順に整列する *)
(* quick_sort : int list -> int list *)

let rec quick_sort lst = 
    (* 目的: lstの中から n より p (不等号)である要素のみを取り出す  *)
    (* take : int -> int list -> (int -> int -> bool) -> int list *)
    let take n lst p = List.filter (fun item -> p item n) lst in

    (* 目的: lstの中から n より小さい要素のみを取り出す *)
    (* take_less : int -> int list -> int list *)
    let take_less n lst = take n lst (<) in

    (* 目的: lstの中から n より大きい要素のみを取り出す *)
    (* take_greater : int -> int list -> int list  *)
    let take_greater n lst = take n lst (>) in

    (* 目的: lstの中から n と同じ要素のみを取り出す *)
    (* take_equal : int -> int list -> int list *)
    let take_equal n lst = take n lst (=) in

    match lst with
        [] -> []
      | first :: rest -> quick_sort (take_less first rest)
                         @ take_equal first lst
                         @ quick_sort (take_greater first rest)

(* テスト *)
let quick_sort_t1 = quick_sort [] = []
let quick_sort_t2 = quick_sort [2] = [2]
let quick_sort_t3 = quick_sort [2; 1] = [1; 2]
let quick_sort_t4 = quick_sort [3; 2; 4; 5; 1; 6] = [1; 2; 3; 4; 5; 6]

let quick_sort_t5 = quick_sort [3; 3; 3; 3; 1; 6] = [1; 3; 3; 3; 3; 6]
let quick_sort_t6 = quick_sort [1; 1] = [1; 1]


(* 問題 15.2: ふたつの自然数 m と n の最大公約数を求める
アルゴリズムとしてはユークリッドの互除法が有名である。
入力は m >= n >= 0 なる自然数 m と n でアルゴリズムは以下のように動く

(1) n = 0 ならば m が最大公約数
(2) そうで無いなら n と 「m を n で割った余り」の最大公約数が求める最大公約数。

これに基づいて、ふたつの自然数を受け取ったら、その最大公約数を返す関数
gcd をデザインレシピにしたがって作れ。
その際、再帰の停止性についてもきちんと議論せよ。 *)

(* 目的: ふたつの自然数を受け取ったら、その最大公約数を返す関数 *)
(* gcd : int -> int -> int *)
let rec gcd m n = 
    if n = 0 then m
    else gcd n ( m mod n )

(* テスト *)
let gcd_t1 = gcd 0 0 = 0
let gcd_t2 = gcd 1 0 = 1
let gcd_t3 = gcd 1 1 = 1
let gcd_t4 = gcd 4 2 = 2
let gcd_t5 = gcd 36 6  = 6
let gcd_t6 = gcd 36 24  = 12


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


