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





