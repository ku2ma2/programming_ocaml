(* 問題 7.1: 国語、数学、英語、理科、社会 の 5教科の点数を与えられたら、
その合計点と平均点を組みにして返す関数 goukei_to_heikin を定義せよ *)
(* 目的: 国語、数学、英語、理科、社会 の 5教科の点数を与えられたら、
その合計点と平均点を組みにして返す関数 *)
(* goukei_to_heikin : int -> int -> int -> int -> int -> int * int *)
let goukei_to_heikin kokugo suugaku eigo rika shakai = 
    let goukei = kokugo + suugaku + eigo + rika + shakai in 
        ( goukei, goukei / 5 ) 

(* テスト *)
let goukei_to_heikin_test1 = goukei_to_heikin 100 100 100 100 100 = (500, 100)
let goukei_to_heikin_test2 = goukei_to_heikin 0 0 0 0 0 = (0, 0)
let goukei_to_heikin_test3 = goukei_to_heikin 70 80 90 100 100 = (440, 88)


(* 問題 7.2: 名前と成績の組を受け取ったら「◯◯さんの成績は△です」
という文字列を返す関数 seiseki を定義せよ *)
(* 目的: 名前と成績の組を受け取ったら「◯◯さんの成績は△です」
という文字列を返す関数 *)
(* seiseki : string * string -> string *)
let seiseki member = match member with
    (name, score) -> name ^ "さんの成績は" ^ score ^ "です"

(* テスト *)
let seiseki_test1 = seiseki ("太郎", "A") = "太郎さんの成績はAです"
let seiseki_test2 = seiseki ("花子", "B") = "花子さんの成績はBです"
