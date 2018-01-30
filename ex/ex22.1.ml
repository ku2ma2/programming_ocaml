(* 問題 22.1: 文字列を受け取ったら、その文字列呼ばれるごとに異なる
数字をつけた文字列を返す関数 gensym をデザインレシピにしたがって作れ。
例えば、最初に gensym "a" とすると "a0" が返り、もう一度
gensym "a" とすると "a1" が返る。さらに gensym "x" とすると "x2"が返る *)

(* グローバル変数 *)
let count = ref (-1)

(* gensym : string -> string *)
let gensym s = 
    (
        count := !count + 1;
        s ^ (string_of_int !count)
    )


(* テスト *)
let gensym_t1 = gensym "a" = "a0"
let gensym_t2 = gensym "a" = "a1"
let gensym_t3 = gensym "x" = "x2"

