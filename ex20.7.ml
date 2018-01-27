(* 問題 20.7: 集合を表すモジュールを作って見よう。以下のシグネチャをもつモジュール Set を実装せよ。 *)

module Set : sig

type 'a t                        (* 要素の型が ' a の集合の型 *)

val empty : 'a t                 (* 空集合 *)
val singleton : 'a -> 'a t       (* 要素ひとつからなる集合 *)
val union : 'a t -> 'a t -> 'a t (* 和集合 *)
val inter : 'a t -> 'a t -> 'a t (* 共通部分 *)
val diff : 'a t -> 'a t -> 'a t  (* 差集合 *)
val mem : 'a t -> 'a t -> bool   (* 要素が集合に入っているか *)

end = struct

type 'a t = 'a list

(* 空集合 *)
(* empty : 'a t *)
let empty = []

(* 要素ひとつからなる集合 *)
(* singleton : 'a -> 'a t *)
let singleton element = [element]

(* 和集合 *)
(* union : 'a t -> 'a t -> 'a t *)
let union g1 g2 = g1 @ g2

(* 共通部分 *)
(* inter : 'a t -> 'a t -> 'a t *)
let inter g1 g2 = 
  List.fold_left
    (fun lst element ->
      if List.mem element g2 then element :: lst else lst
    )
    empty g1

(* 差集合 *)
(* diff : 'a t -> 'a t -> 'a t *)
let diff g1 g2 = 
  List.fold_left
    (fun lst element ->
      if List.mem element g2 then lst else element :: lst
    )
    empty g1

(* 要素が集合に入っているか *)
(* mem : 'a t -> 'a t -> bool *)
let mem element set = List.mem element set


end