(* 問題 6.1: 次のプログラムを実行するとどのようなエラーになるか予想せよ。
実際にOCamlインタプリタに入力して確認せよ *)

let square x = x * x ;;
(* square 3.;;  *)
(* Error: This expression has type float but an expression was expected of type *)

(* let circle = 2. *. pi *. r;;
circle 2.;; *)
(* Error: Unbound value pi *)

(* let TV bangumi youbi = bangumi ^ "は" ^ youbi ^ "に放映です。";;
TV "ポケモン" "土曜日";; *)
(* Error: Syntax error *)

