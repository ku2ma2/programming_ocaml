(* 問題 16.1: 整数のリストを受け取ったら、それまでの数の合計からなる
リストを返す関数 sum_list をデザインレシピニしたがって作れ。
例えば sum_list [3; 2; 1; 4] は [3; 5; 6; 10] を返す *)
(* sum_list : int list -> int list *)
let sum_list lst = 

    (* アキュムレータ: totalがこれまでの合計  *)
    (* sum_sub : int list -> int -> int list *)
    let rec sum_sub sum_list total = match sum_list with
        [] -> []
      | first :: rest -> (total + first) :: sum_sub rest (total + first)

    in sum_sub lst 0

(* テスト *)
let sum_list_t1 = sum_list [] = []
let sum_list_t2 = sum_list [3; 2; 1; 4] = [3; 5; 6; 10]
let sum_list_t3 = sum_list [3] = [3]
let sum_list_t4 = sum_list [3; 3] = [3; 6]


(* 問題 16.2: 関数 fと 初期値 init, そして リスト lst を受け取ったら、
initからはじめてリスト lst の要素を「左から」順に f を施し込む関数
fold_let をデザインレシピに従って作れ *)
let rec fold_left f ini lst = match lst with
    [] -> ini
  | first :: rest -> fold_left f (f ini first) rest

(* テスト *)
let fold_left_t1 = fold_left (-) 0 [] = 0 
let fold_left_t2 = fold_left (-) 10 [4; 1; 3] = 2 
let fold_left_t3 = fold_left (fun lst a -> a :: lst) [] [1; 2; 3; 4] = [4; 3; 2; 1] 

