
(* 目的: 受け取った lst の中の最小値を返す *)
(* minimum : int list -> int *)
let rec minimum_loop start lst = match lst with
    [] -> start
  | first :: rest ->
      let minimum_rest = minimum_loop first rest in
        if minimum_rest <= start then minimum_rest
        else start

 
(* 目的：受け取った lst の中の最小値を返す *) 
(* minimum : int list -> int *) 
let minimum lst = match lst with 
    [] -> max_int 
  | first :: rest -> minimum_loop first rest 

(* テスト *)
let test0 = minimum [] = max_int
let test1 = minimum [3] = 3
let test2 = minimum [1; 2] = 1
let test3 = minimum [3; 2] = 2
let test4 = minimum [3; 2; 6; 4; 1; 8] = 1
