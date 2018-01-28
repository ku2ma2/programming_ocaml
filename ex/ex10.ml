#use "ex8.ml";;

(* 問題 10.1: あらかじめ昇順に並んでいる整数のリスト lst と整数 n を受け取ったら、
lst　を前から順に見ていき、昇順となる位置に n を挿入したリストを返す関数 insert
をデザインレシピにしたがって作れ、例えば insert [1; 3; 4; 7; 8] 5 は 
[1; 3; 4; 5; 7; 8] を返す  *)
(* insert : int list -> int -> int list *)
let rec insert lst n = match lst with
    [] -> n :: []
  | first :: rest -> 
      if first < n then first :: insert rest n
      else n :: lst

(* テスト *)
let insert_test1 = insert [] 1 = [1]
let insert_test2 = insert [1] 3 = [1; 3]
let insert_test3 = insert [3] 1 = [1; 3]
let insert_test4 = insert [1; 3; 4; 7; 8] 5 = [1; 3; 4; 5; 7; 8]

(* 問題 10.2: 整数のリストを受け取ったら、それを昇順に整列したリストを返す関数
ins_sort をデザインレシピに従って作れ 例えば ins_sort [5; 3; 8; 1; 7; 4] は
[1; 3; 4; 5; 7; 8] を返す。問題 10.1で作った insert 使用して構わない。 *)
(* ins_sort : int list -> int list *)
let rec ins_sort lst = match lst with
    [] -> []
  | first :: rest -> insert (ins_sort rest) first

(* テスト *)
let ins_sort_test1 = ins_sort [] = []
let ins_sort_test2 = ins_sort [3] = [3]
let ins_sort_test2 = ins_sort [3; 1] = [1; 3]
let ins_sort_test2 = ins_sort [5; 3; 8; 1; 7; 4] = [1; 3; 4; 5; 7; 8]


(* 問題 10.3: gakusei_t 型のリストを受け取ったら、それを tensuu フィールドの順に
整列したリストを返す関数 gakusei_sort をデザインレシピにしたがって作れ *)

(* 学生型定義 *)
type gakusei_t = { 
  namae : string;       (* 名前 *) 
  tensuu : int;         (* 点数 *) 
  seiseki : string;     (* 成績 *) 
} 

(* 学生テストデータ *) 
let gakusei1 = {namae="nakamura"; tensuu=90; seiseki="A"} 
let gakusei2 = {namae="miyahara"; tensuu=80; seiseki="A"} 
let gakusei3 = {namae="sato"; tensuu=75; seiseki="B"} 
let gakusei4 = {namae="idehara"; tensuu=70; seiseki="B"} 
let gakusei5 = {namae="tsubata"; tensuu=65; seiseki="C"} 
let gakusei6 = {namae="asai"; tensuu=60; seiseki="C"} 
 
(* テスト用 学生リスト *) 
let gakusei_lst1 = [] 
let gakusei_lst2 = [gakusei3; gakusei1] 
let gakusei_lst3 = [gakusei3; gakusei2] 
let gakusei_lst4 = [gakusei4; gakusei3] 
let gakusei_lst5 = [gakusei4; gakusei1; gakusei6; gakusei5; gakusei2; gakusei3] 

(* gakusei_insert : gakusei_t list -> gakusei_t -> gakusei_t list *)
let rec gakusei_insert lst gakusei = match lst with
    [] -> [gakusei]
  | ({namae=n; tensuu=t; seiseki=s} as first):: rest ->
      match gakusei with
          {namae=n0; tensuu=t0; seiseki=s0} ->
              if t < t0 then first :: gakusei_insert rest gakusei
              else gakusei :: lst

(* テスト *)
let gakusei_insert_t1 = gakusei_insert gakusei_lst1 gakusei2 = [gakusei2]
let gakusei_insert_t2 = gakusei_insert gakusei_lst2 gakusei2 = [gakusei3; gakusei2; gakusei1]

(* gakusei_sort : gakusei_t list -> gakusei_t list *)
let rec gakusei_sort lst = match lst with
    [] -> []
  | first :: rest -> gakusei_insert (gakusei_sort rest) first

(* テスト *)
let gakusei_sort_t1 = gakusei_sort gakusei_lst1 = []
let gakusei_sort_t2 = gakusei_sort gakusei_lst2 = [gakusei3; gakusei1]
let gakusei_sort_t3 = gakusei_sort gakusei_lst3 = [gakusei3; gakusei2]
let gakusei_sort_t3 = gakusei_sort gakusei_lst4 = [gakusei4; gakusei3]
let gakusei_sort_t3 = gakusei_sort gakusei_lst5 = [gakusei6; gakusei5; gakusei4; gakusei3; gakusei2; gakusei1]


(* 問題 10.4: 問題 8.3 で定義した person_t 型のリストを受け取ったら、それを
名前の順に整列したリストを返す関数 person_sort をデザインレシピにしたがって作れ。 *)

(* 名簿リスト *)
let person_lst1 = []
let person_lst2 = [person_1]
let person_lst3 = [person_3; person_4]
let person_lst4 = [person_1; person_2; person_3; person_4]

(* 目的: あらかじめ名前で整列している person_t 型のリストに person_t を
名前の順番位置に挿入する 関数 *)
(* person_insert : person_t list -> person_t -> person_t list *)
let rec person_insert lst person = match lst with
    [] -> [person]
  | ({name=n; height=h; weight=w; birthday=b; blood=bl} as first) :: rest -> 
      match person with
          {name=n0; height=h0; weight=w0; birthday=b0; blood=bl0} -> 
              if n < n0 then first :: person_insert rest person
              else person :: lst

(* テスト *)
let person_insert_t1 = person_insert person_lst1 person_1 = [person_1]
let person_insert_t2 = person_insert person_lst2 person_2 = [person_2; person_1]
let person_insert_t3 = person_insert person_lst3 person_2 = [person_3; person_2; person_4]


(* 目的: person_t 型のリストを受けとったら名前の順に整列したリストを返す関数 *)
(* person_sort : person_t list -> person_t list *)
let rec person_sort lst = match lst with
    [] -> []
  | first :: rest ->  person_insert (person_sort rest) first

(* テスト *)
let person_sort_t1 = person_sort person_lst1 = []
let person_sort_t2 = person_sort person_lst2 = [person_1]
let person_sort_t3 = person_sort person_lst3 = [person_3; person_4]
let person_sort_t4 = person_sort person_lst4 = [person_3; person_2; person_4; person_1]

(* 問題 10.5: gakusei_t 型のリストを受け取ったら、その中から最高得点を取った人の
レコードを返す関数 gakusei_max をデザインレシピにしたがって作れ *)
(* 問題 10.6: 問題 10.5 で作った関数 gakusei_max を書き直して、
同じ計算を2度することがないようにせよ。 *)

(* gakusei_max : gakusei_t list -> gakusei_t *)
let rec gakusei_max lst = match lst with
    [] -> {namae=""; tensuu=min_int; seiseki=""} 
  | ({namae=n; tensuu=t; seiseki=s} as first) :: rest -> 
      let rest_max = gakusei_max rest in 
        match rest_max with
        {namae=n0; tensuu=t0; seiseki=s0} ->
          if t > t0 then first
          else rest_max


(* テスト用 学生リスト *) 
let gakusei_max_lst1 = [] 
let gakusei_max_lst2 = [gakusei3; gakusei1]
let gakusei_max_lst3 = [gakusei2; gakusei3]
let gakusei_max_lst4 = [gakusei4; gakusei1; gakusei6; gakusei5; gakusei2; gakusei3] 

(* テスト *)
let gakusei_max_t1 = gakusei_max gakusei_max_lst1 = {namae=""; tensuu=min_int; seiseki=""} 
let gakusei_max_t2 = gakusei_max gakusei_max_lst2 = gakusei1
let gakusei_max_t3 = gakusei_max gakusei_max_lst3 = gakusei2
let gakusei_max_t4 = gakusei_max gakusei_max_lst4 = gakusei1

(* 問題 10.7: 問題 8.3で定義した person_t 型のデータのリストを受け取ったら、
各血液型の人が何人いるのかを組にして返す関数 ketsueki_shukei を
デザインレシピにしたがって作れ。 *)
(* ketsueki_shukei : person_t list -> int * int * int * int *)
let rec ketsueki_shukei lst = match lst with
    [] -> (0, 0, 0, 0)
  | {name=n; height=h; weight=w; birthday=bi; blood=bl} :: rest -> 
        let (a, b, o, ab) = ketsueki_shukei rest in
            if bl = "A型" then (a + 1, b, o, ab)
            else if bl = "B型" then (a, b + 1, o, ab)
            else if bl = "O型" then (a, b, o + 1, ab)
            else if bl = "AB型" then (a, b, o, ab + 1)
            else (a, b, o, ab)

let person_lst1 = []
let person_lst2 = [person_1]
let person_lst3 = [person_1; person_2; person_3; person_4]

(* テスト *)
let ketsueki_shukei_t1 = ketsueki_shukei person_lst1 = (0, 0, 0, 0)
let ketsueki_shukei_t2 = ketsueki_shukei person_lst2 = (0, 0, 1, 0)
let ketsueki_shukei_t3 = ketsueki_shukei person_lst3 = (2, 0, 1, 1)

(* 問題 10.8: person_t 型のデータのリストを受け取ったら、
4つの血液型のうち最も人数の多かった血液型を返す関数 saita_ketsueki
をデザインレシピにしたがって作れ *)
(* saita_ketsueki : person_t list -> string *)
let saita_ketsueki lst = 
    let (a, b, o, ab) = ketsueki_shukei lst in
      let saidai = max ( max a b ) ( max o ab ) in
        if saidai = a then "A型"
        else if saidai = b then "B型"
        else if saidai = o then "O型"
        else if saidai = ab then "AB型"
        else "なし"

(* テスト *)
let saita_ketsueki_t1 = saita_ketsueki person_lst1 = "A型" (* 同点の場合は見つかった順になる *)
let saita_ketsueki_t2 = saita_ketsueki person_lst2 = "O型"
let saita_ketsueki_t3 = saita_ketsueki person_lst3 = "A型"

(* 問題 10.9: ふたつのりストを受け取ってきたら、それらの長さが同じかどうかを判定する関数
equal_length をデザインレシピにしたがって作れ（関数 length は使わずに作成せよ） *)
(* equal_length : 'a list -> 'a list -> bool *)
let rec equal_length lst1 lst2 = match (lst1, lst2) with
    ([], []) -> true
  | ([], first2 :: rest2) -> false
  | (first1 :: rest1, []) -> false
  | (first1 :: rest1, first2 :: rest2) ->
      equal_length rest1 rest2 

(* テスト *)
let equeal_length_t1 = equal_length [] [] = true
let equeal_length_t2 = equal_length [] [2] = false
let equeal_length_t3 = equal_length [1] [] = false
let equeal_length_t4 = equal_length [3; 4; 6] [5; 6; 7] = true
let equeal_length_t5 = equal_length [3; 4] [5; 6; 7] = false
let equeal_length_t6 = equal_length ["a"; "b"] [5; 6; 7] = false
let equeal_length_t7 = equal_length ["a"; "b"; "c"] [5; 6; 7] = true
