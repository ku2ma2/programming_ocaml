(* オプション型 *)
type 'a option =
    None
  | Some of 'a

(* 問題 18.2: 野菜のリストと八百屋のリストを受け取ったら、野菜のリスト
のうち八百屋に置いていない野菜の数を返す関数 count_urikire_yasai
をデザインレシピにしたがって作れ。 *)

(* テスト用八百屋 *)
let yaoya_list = [
    ("トマト", 300);
    ("たまねぎ", 200);
    ("にんじん", 150);
    ("ほうれん草", 200)
]

(* 目的: 野菜の名前と八百屋のリストを受け取ったらその野菜が八百屋のリスト
にあるかどうかを調べる *)
(* is_yasai : string -> (string * int) list -> (string * int) *)
let rec is_yasai yasai yaoya = match yaoya with
    [] -> None
  | (name, price) :: rest -> 
        if name = yasai then Some(name, price)
        else is_yasai yasai rest

(* is_yasai テスト *)
let is_yasai_t1 = is_yasai "トマト" yaoya_list = Some ("トマト", 300)
let is_yasai_t2 = is_yasai "ヤーコン" yaoya_list = None
let is_yasai_t3 = is_yasai "にんじん" yaoya_list = Some ("にんじん", 150)


(* 目的: 野菜のリストと八百屋のリストを受け取ったら、野菜のリスト
のうち八百屋に置いていない野菜の数を返す関数 *)
(* count_urikire_yasai : string list -> (string * int) list -> int *)
let rec count_urikire_yasai yasai_lst yaoya = match yasai_lst with
    [] -> 0
  | first :: rest -> 
        match is_yasai first yaoya with
            None -> 1 + count_urikire_yasai rest yaoya
          | Some (name, price) -> count_urikire_yasai rest yaoya


(* count_urikire_yasai テスト *)
let yasai_lst1= []
let yasai_lst2= ["トマト"; "たまねぎ"; "にんじん"; "ほうれん草"]
let yasai_lst3= ["たまねぎ"; "ほうれん草"]
let yasai_lst4= ["たまねぎ"; "ヤーコン"; "ほうれん草"; "じゃがいも"]

let count_urikire_yasai_t1 = count_urikire_yasai yasai_lst1 yaoya_list = 0
let count_urikire_yasai_t2 = count_urikire_yasai yasai_lst2 yaoya_list = 0
let count_urikire_yasai_t3 = count_urikire_yasai yasai_lst3 yaoya_list = 0
let count_urikire_yasai_t4 = count_urikire_yasai yasai_lst4 yaoya_list = 2
