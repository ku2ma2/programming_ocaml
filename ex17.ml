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


(* 問題 17.2: 1月から12月までを表す構成子 January, ..., December を持つ型
year_t を宣言せよ。構成子には日を示す引数を取るようにせよ。 *)
type year_t =
    January of int   (* 1月 *)
  | February of int  (* 2月 *)
  | March of int     (* 3月 *)
  | April of int     (* 4月 *)
  | May of int       (* 5月 *)
  | June of int      (* 6月 *)
  | July of int      (* 7月 *)
  | August of int    (* 8月 *)
  | September of int (* 9月 *)
  | October of int   (* 10月 *)
  | November of int  (* 11月 *)
  | December of int  (* 12月 *)

(* 問題 17.3: 12星座を表す siza_t を宣言せよ *)
type seiza_t = 
  Capricorus  (* 山羊座 *)
| Aquarius    (* 水瓶座 *)
| Pisces      (* 魚座 *)
| Aries       (* 牡羊座 *)
| Taurus      (* 牡牛座 *)
| Gemini      (* 双子座 *)
| Cancer      (* 蟹座 *)
| Leo         (* 獅子座 *)
| Virgo       (* 乙女座 *)
| Libra       (* 天秤座 *)
| Scorpius    (* 蠍座 *)
| Sagittarius (* 射手座 *)



