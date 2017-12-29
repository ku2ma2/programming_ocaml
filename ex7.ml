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

(* 問題 7.3: x座標とy座標の組で表された平面座標を受け取ったら、x軸について
対称な点の座標を返す関数 taisho_x をデザインレシピにしたがって作れ。 *)
(* 目的: x座標とy座標の組で表された平面座標を受け取ったら、
x軸について対称な点の座標を返す関数 *)
(* taisho_x : float * float -> float * float *)
let taisho_x point = match point with
    (x, y) -> (x, -. y)

(* テスト *)
let taisho_x_test1 = taisho_x (100.0, 200.0) = (100.0, -200.0)
let taisho_x_test2 = taisho_x (0.0, 0.0) = (0.0, 0.0)
let taisho_x_test3 = taisho_x (50.0, 300.0) = (50.0, -300.0)

(* 問題 7.4: x座標と y座標の組で表された平面座標をふたつ受けとったら、
その中点の座標を返す関数 chuten をデザインレシピにしたがって作れ *)
(* 目的: x座標と y座標の組で表された平面座標をふたつ受けとったら、
その中点の座標を返す関数 *)
(* chuten : float * float -> float * float -> float * float *)
let chuten start_point end_point = match start_point with
    (x1, y1) -> match end_point with
        (x2, y2) -> ((x1 +. x2) /. 2.0, (y1 +. y2) /. 2.0)

(* テスト *)
let chuten_test1 = chuten (0.0, 0.0) (100.0, 100.0) = (50.0, 50.0)
let chuten_test2 = chuten (0.0, 0.0) (0.0, 0.0) = (0.0, 0.0)
let chuten_test3 = chuten (50.0, -22.0) (10.0, 30.0) = (30.0, 4.0)

