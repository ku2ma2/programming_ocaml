(* 問題 5.1: 次の if 文にはどのような型がつくか。つかない場合、どこで型エラーが起きるか *)
if 2 < 1 then 3 else 4;; (* int = 4 *)
(* if "true" then 3.14 else 2.72;; ERROR *)
if "a" = "b" then false else true;;
(* if true < false then 1 else "2";; ERROR *)
if not (3 = 4) then 1 < 2 else 1 > 2;;

