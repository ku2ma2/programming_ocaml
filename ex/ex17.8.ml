(* 問題 17.9: 多相の木の型を使って 17.3節で作った関数 sum_tree を定義せよ。
この関数の型は何になるか。なぜこの関数が受け取る木の要素の型はそ多相にならないのか *)

(* 多相の木を表す型 *)
type 'a tree_t = 
    Empty                           (* 空の木 *)
  | Leaf of 'a                         (* 葉 *)
  | Node of 'a tree_t * 'a * 'a tree_t (* 節 *)

(* 目的: tree に含まれる整数をすべて加える *)
(* sum_tree : tree_t -> int *)
let rec sum_tree tree = match tree with
    Empty -> 0
  | Leaf (n) -> n
  | Node (ln, n, rn) -> sum_tree ln + n + sum_tree rn

(* テスト用の木 *)
let tree1 = Empty
let tree2 = Leaf (3)
let tree3 = Node (tree1, 4, tree2)
let tree4 = Node (tree2, 5, tree3)

(* テスト *)
let test1 = sum_tree tree1 = 0
let test2 = sum_tree tree2 = 3
let test3 = sum_tree tree3 = 7
let test4 = sum_tree tree4 = 15
