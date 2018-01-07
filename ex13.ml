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


(* 問題 13.2: 問題8.3で定義した person_t 型のリストを受け取ったら、
その中に出てくる人の名前のリストを返す関数 person_namae を
デザインレシピに従って作れ。 *)
(* take_namae : person_t -> string *)
let take_namae person = match person with
    {name=namae; height=h; weight=w; birthday=b; blood=bl} -> namae

(* テスト *)
let take_namae_t1 = take_namae person_1 = "太郎"
let take_namae_t2 = take_namae person_2 = "Jiro"
let take_namae_t3 = take_namae person_3 = "Hanako"

(* person_namae : person_t list -> string list *)
let person_namae lst = List.map take_namae lst

(* テスト *)
let person_name_t1 = person_namae person_list_t1 = []
let person_name_t2 = person_namae person_list_t2 = ["太郎"; "Jiro"; "Hanako"; "Junko"]


(* 問題 13.3: 次の型を持つ関数を定義せよ *)

(* (1) 'a -> 'a *)
let func1 a = a

(* (2) 'a -> 'b -> 'a *)
let func2 a b = a

(* (3) 'a -> 'b -> 'b *)
let func3 a b = b

(* (4) 'a -> ('a -> 'b) -> 'b *)
let func4 a b = b a

(* (5) ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c *)
let func5 f g x = g (f x)
