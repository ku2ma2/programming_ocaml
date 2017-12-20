(* 問題 4.1: アルバイトを始めた時には時給 850 円だが
1年経過するごとに時給が 100 円ずつ上がることにしよう。
アルバイトを始めてからの年数とその月に働いた時間を与えられたら、
その月の給与を返す baito_kyuyo を定義せよ *)
let baito_kyuyo nensuu jikan = (850 + (nensuu * 100) ) * jikan

let baito_kyuyo_test1 = baito_kyuyo 0 10 = 8500
let baito_kyuyo_test2 = baito_kyuyo 1 10 = 9500
let baito_kyuyo_test3 = baito_kyuyo 10 1 = 1850

(* 問題 4.2: 名前を与えられたら、その名前を使った
紹介文を返す関数 jikoshokai を定義せよ *)
let jikoshokai name = "私は" ^ name ^ "です。よろしくお願いします。"

let jikoshokai_test1 = jikoshokai "二郎" = "私は二郎です。よろしくお願いします。"
let jikoshokai_test2 = jikoshokai "三郎" = "私は三郎です。よろしくお願いします。"

(* 問題 4.3: 身長(m) を与えられたら、標準体重を返す
関数 hyojun_taiju を定義せよ。
ここで標準体重は身長の 2乗 に 22 をかけることで得られる。 *)
let hyojun_taiju tall = tall ** 2.0 *. 22.0

let hyojun_taiju_test1 = hyojun_taiju 1.0 = 22.0
let hyojun_taiju_test2 = hyojun_taiju 1.2 = 31.68

(* 問題 4.4: 身長(m) と 体重(kg) を与えられたら、
BMI指数 を返す関数 bmi を定義せよ。
BMI指数は体重を身長の2乗で割ることで得られる *)
let bmi tall weight = weight /. ( tall ** 2.0 )

let bmi_test1 = bmi 1.0 30.0 = 30.0
let bmi_test2 = bmi 2.0 100.0 = 25.0



