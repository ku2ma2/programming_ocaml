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

end