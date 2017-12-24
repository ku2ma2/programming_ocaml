(* 問題 5.1: 次の if 文にはどのような型がつくか。つかない場合、どこで型エラーが起きるか *)
if 2 < 1 then 3 else 4;; (* int = 4 *)
(* if "true" then 3.14 else 2.72;; ERROR *)
if "a" = "b" then false else true;;
(* if true < false then 1 else "2";; ERROR *)
if not (3 = 4) then 1 < 2 else 1 > 2;;

(* 問題 5.2: 時間を受け取ったら、午前か午後かを返す関数jikan を
デザインレシピにしたがって作れ *)

(* 目的: 午前か午後かを返す関数 *)
(* jikan: int -> string *)
let jikan hour = 
    if hour < 12 then "午前"
                 else "午後"

(* テスト *)
let jikan_test1 = jikan 0 = "午前"
let jikan_test2 = jikan 3 = "午前"
let jikan_test3 = jikan 12 = "午後"
let jikan_test4 = jikan 13 = "午後"

(* 問題 5.3: 誕生日（月と日）を受け取ったら、星座を返す関数 seiza を
デザインレシピにしたがって作れ *)
(* seiza : int -> int -> string *)

(* 目的: 誕生日（月と日）を受け取ったら、星座を返す関数 *)
let seiza month day = 
    if month = 1 then if 1 <= day && day <= 19 then "山羊座"
                      else if 20 <= day && day <= 31 then "水瓶座"
                      else "該当なし"
    else if month = 2 then if 1 <= day && day <= 18 then "水瓶座"
                      else if 19 <= day && day <= 29 then "魚座"
                      else "該当なし"
    else if month = 3 then if 1 <= day && day <= 20 then "魚座"
                      else if 21 <= day && day <= 31 then "牡羊座"
                      else "該当なし"
    else if month = 4 then if 1 <= day && day <= 19 then "牡羊座" 
                      else if 20 <= day && day <= 30 then "牡牛座" 
                      else "該当なし" 
    else if month = 5 then if 1 <= day && day <= 20 then "牡牛座" 
                      else if 21 <= day && day <= 31 then "双子座" 
                      else "該当なし" 
    else if month = 6 then if 1 <= day && day <= 21 then "双子座" 
                      else if 22 <= day && day <= 30 then "蟹座" 
                      else "該当なし" 
    else if month = 7 then if 1 <= day && day <= 22 then "蟹座" 
                      else if 23 <= day && day <= 31 then "獅子座" 
                      else "該当なし" 
    else if month = 8 then if 1 <= day && day <= 22 then "獅子座" 
                      else if 23 <= day && day <= 31 then "乙女座" 
                      else "該当なし" 
    else if month = 9 then if 1 <= day && day <= 22 then "乙女座" 
                      else if 23 <= day && day <= 30 then "天秤座" 
                      else "該当なし" 
    else if month = 10 then if 1 <= day && day <= 23 then "天秤座" 
                       else if 24 <= day && day <= 31 then "蠍座" 
                       else "該当なし" 
    else if month = 11 then if 1 <= day && day <= 21 then "蠍座" 
                       else if 22 <= day && day <= 30 then "射手座" 
                       else "該当なし" 
    else if month = 12 then if 1 <= day && day <= 21 then "射手座" 
                       else if 22 <= day && day <= 31 then "山羊座" 
                       else "該当なし" 
    else "該当なし"

(* テスト *)
let seiza_test1 = seiza 3 21 = "牡羊座"
let seiza_test2 = seiza 6 12 = "双子座"
let seiza_test3 = seiza 7 23 = "獅子座"
let seiza_test4 = seiza 12 21 = "射手座"


