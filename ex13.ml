#use "ex8.ml";;

(* 問題 13.1: 問題8.3 で定義した person_t 型のリストを受け取ったら、
その中から指定された血液型の人の数を返す関数 count_ketsueki を
デザインレシピにしたがって作れ *)
(* count_ketsueki : person_t list -> string -> int *)
let rec count_ketsueki lst blood = match lst with
    [] -> 0
  | {name=n; height=h; weight=w; birthday=bi; blood=b} :: rest -> 
      if b = blood then 1 + count_ketsueki rest blood
      else count_ketsueki rest blood

(* テスト *)
let person_list_t1 = []
let person_list_t2 = [person_1; person_2; person_3; person_4]

let count_ketsueki_t1 = count_ketsueki person_list_t1 "A型" = 0
let count_ketsueki_t2 = count_ketsueki person_list_t2 "A型" = 2
let count_ketsueki_t3 = count_ketsueki person_list_t2 "B型" = 0
let count_ketsueki_t4 = count_ketsueki person_list_t2 "AB型" = 1

