
(* 赤か黒を示す型 *)
type color_t = Red | Black

(* 問題20.1: 各節に 'a　型のキーと 'b 型の値、そして色を示す
color_t 型の値を持つ木の型 rb_tree_t を宣言せよ。 *)
(* 多相型の木になる 17.5 (p191)参照 *)
type ('a, 'b) rb_tree_t = 
    Empty
  | Node of ('a, 'b) rb_tree_t * 'a * 'b * color_t * ('a, 'b) rb_tree_t

(* 空の木 *)
let empty = Empty

(* balance : ('a, 'b) rb_tree_t -> ('a, 'b) rb_tree_t *)
let balance rb_tree = match rb_tree with
    Node (Node (Node (a, xa, xb, Red, b), ya, yb, Red, c), za, zb, Black, d) 
  | Node (Node (a, xa, xb, Red, Node (b, ya, yb, Red, c)), za, zb, Black, d) 
  | Node (a, xa, xb, Black, Node (Node (b, ya, yb, Red, c), za, zb, Red, d)) 
  | Node (a, xa, xb, Black, Node (b, ya, yb, Red, Node (c, za, zb, Red, d))) 
        -> Node (Node (a, xa, xb, Black, b), ya, yb, Red, Node (c, za, zb, Black, d)) 
  | _ -> rb_tree

(* 目的: tree にキーが k で値が　v を挿入した木を返す *)
(* insert : ('a, 'b) rb_tree_t -> 'a -> 'b -> ('a, 'b) rb_tree_t *)
let insert rb_tree k v = 
  let rec ins rb_tree = match rb_tree with 
    Empty -> Node (Empty, k, v, Red, Empty) 
    | Node (left, key, value, color, right) -> 
        if k = key 
          then Node (left, k, v, color, right) 
        else if k < key 
          then balance (Node (ins left, key, value, color, right)) 
          else balance (Node (left, key, value, color, ins right)) 
    in match ins rb_tree with
      Empty -> assert false
      | Node (left, key, value, color, right) -> 
        Node (left, key, value, Black, right)

(* 目的 : tree の中のキー k に対応する値を探して返す *)
(* みつからなければ例外 Not_found を起こす *)
(* search : ('a, 'b) t -> 'a -> 'b *)
let rec search rb_tree k = match rb_tree with
    Empty -> raise Not_found
  | Node (left, key, value, color, right) ->
    if k = key then value
    else if k < key then search left k
                    else search right k
