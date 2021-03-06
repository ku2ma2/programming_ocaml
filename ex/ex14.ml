#use "ex8.ml";;

(* 問題 14.1: 問題 9.5で作成した関数 even を filter 
を用いて定義せよ。 *)

(* 目的: 整数のリストを受け取ったら、その中の偶数の要素のみを含むリストを返す関数
例えば even [2; 1; 6; 4; 7] は [2; 6; 4] を返す *)
(* even : int list -> int list *)
let rec even lst = 
    List.filter (fun x -> x mod 2 = 0) lst

(* テスト *)
let even_test1 = even [] = []
let even_test2 = even [2] = [2]
let even_test3 = even [2; 1; 6; 4; 7] = [2; 6; 4]


(* 問題 14.3: 問題 9.6 で作成した関数 concat を fold_right を使って書き直せ *)
(* 問題 14.14: 問題 9.6 または 問題 14.3 で作成した関数 concat を 1行で定義せよ *)

(* 目的: init から始めて lst の要素を右から順に f を施しこむ *)
(* fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
(* let rec fold_right f lst init = match lst with
    [] -> init
  | first :: rest -> f first (fold_right f rest init) *)

(* concat : string list -> string *)
let concat lst = List.fold_right (^) lst ""

(* テスト *)   
let concat_test1 = concat [] = ""
let concat_test2 = concat ["春"] = "春"
let concat_test3 = concat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬"


(* 問題 14.6: 13.1 節に示した関数 count を filter と length 
を用いて定義せよ。 *)
(* count_ketsueki : person_t list -> string -> int *)
let count_ketsueki lst blood = 
    let is_blood person = match person with
        {name=n; height=h; weight=w; birthday=b; blood=bl} -> 
            bl = blood in 
                List.length ( List.filter is_blood lst )

(* テスト *)
let person_list_t1 = []
let person_list_t2 = [person_1; person_2; person_3; person_4]

let count_ketsueki_t1 = count_ketsueki person_list_t1 "A型" = 0
let count_ketsueki_t2 = count_ketsueki person_list_t2 "A型" = 2
let count_ketsueki_t3 = count_ketsueki person_list_t2 "B型" = 0
let count_ketsueki_t4 = count_ketsueki person_list_t2 "AB型" = 1

;;

(* 問題 14.8: 整数を受け取ってきたら、その2乗から 1 を引いた数を返す名前のない関数を作れ *)
fun x -> x * x - 1;;


(* 問題 14.9: 問題8.3で定義した person_t 型のリストを受け取ってきたら、
その名前フィールドを取り出す名前のない関数を作れ *)
(* fun person -> match person with
    {name=n; height=h; weight=w; birthday=b; blood=bl} -> n;; *)
fun {name=n; height=h; weight=w; birthday=b; blood=bl} -> n;;


(* 問題 14.10: 問題 14.5で作成した巻数を名前のない関数を使って書き直せ *)

(* 問題 14.15: 1から受け取った自然数までの合計を求める関数
one_to_n を enumerate と高階関数を使って作れ *)

(* 目的: n から 1までのリストを作る *)
(* enumerate : int -> int list *)
let rec enumerate n = 
    if n = 0 then [] else n :: enumerate (n - 1)

let enumerate_t1 = enumerate 5 = [5; 4; 3; 2; 1]
let enumerate_t3 = enumerate 0 = []

(* 目的: 1から受け取った自然数までの合計を求める関数 *)
(* one_to_n : int -> int *)
let one_to_n n = List.fold_right (+) (enumerate n) 0

(* テスト *)
let one_to_n_t1 = one_to_n 0 = 0
let one_to_n_t2 = one_to_n 1 = 1
let one_to_n_t3 = one_to_n 2 = 3
let one_to_n_t4 = one_to_n 5 = 15

(* 問題 14.16: 階乗を求める関数 fac を enumerate と 高階関数を使って作れ *)
(* fac : int -> int *)
let fac n = List.fold_right ( * ) (enumerate n) 1

(* テスト *)
let fac_t1 = fac 0 = 1
let fac_t2 = fac 1 = 1
let fac_t3 = fac 2 = 2
let fac_t4 = fac 3 = 6
let fac_t5 = fac 6 = 720
let fac_t6 = fac 10 = 3628800


