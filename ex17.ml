(* 問題 17.1: 誕生年と現在の年を nengou_t 型の値として受け取ってきたら、
年齢を返す関数 nenrei をデザインレシピにしたがって作れ。 *)

(* 年号を表す型 *)
type nengou_t = Meiji of int   (* 明治 *)
              | Taisho of int  (* 大正 *)
              | Showa of int   (* 昭和 *)
              | Heisei of int  (* 平成 *)

(* nenrei : nengou_t -> int *)