(* 問題 17.1: 誕生年と現在の年を nengou_t 型の値として受け取ってきたら、
年齢を返す関数 nenrei をデザインレシピにしたがって作れ。 *)

(* 年号を表す型 *)
type nengou_t = Meiji of int   (* 明治 *)
              | Taisho of int  (* 大正 *)
              | Showa of int   (* 昭和 *)
              | Heisei of int  (* 平成 *)

(* 目的: 年号を受け取ったら対応する西暦年を返す *)
(* to_seireki : nengou_t -> int *)
let to_seireki nengou = match nengou with
    Meiji (n) -> n + 1867
  | Taisho (n) -> n + 1911
  | Showa (n) -> n + 1925
  | Heisei (n) -> n + 1988

(* 目的: 誕生年と現在の年を nengou_t 型の値として受け取ってきたら、年齢を返す関数 *)
(* nenrei : nengou_t -> int -> int *)
let nenrei nengou now = 
  to_seireki now - to_seireki nengou


(* テスト *)
let nenrei_t1 = nenrei (Meiji (2)) (Heisei (30)) = 149
let nenrei_t2 = nenrei (Taisho (3)) (Heisei (30)) = 104 
let nenrei_t3 = nenrei (Showa (11)) (Heisei (30)) = 82 
let nenrei_t4 = nenrei (Heisei (18)) (Heisei (30)) = 12 