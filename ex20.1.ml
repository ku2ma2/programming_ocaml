
(* 赤か黒を示す型 *)
type color_t = Red | Black

(* 問題20.1: 各節に 'a　型のキーと 'b 型の値、そして色を示す
color_t 型の値を持つ木の型 rb_tree_t を宣言せよ。 *)
(* 多相型の木になる 17.5 (p191)参照 *)
type ('a, 'b) rb_tree_t = 
    Empty
  | Node of ('a, 'b) rb_tree_t * 'a * 'b * color_t * ('a, 'b) rb_tree_t
  


