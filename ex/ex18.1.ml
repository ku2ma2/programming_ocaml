(* 問題 18.1: 問題 8.3 で定義した person_t 型のリストを受け取ったら、
その中から最初のA型の人のレコードをオプション型で返す関数 first_A
をデザインレシピにしたがって作れ。A型の人がいなかったら None を返すようにせよ。 *)

(* オプション型 *)
type 'a option =
    None
  | Some of 'a

(* person_t 型 *)
type person_t = {
    name: string;
    height: float;
    weight: float;
    birthday:  string;
    blood: string;
}

(* first_A : person_t list -> person_t *)
let rec first_A lst = match lst with
    [] -> None
  | ({name=n; height=h; weight=w; birthday=b; blood=bl} as first) :: rest -> 
      if bl = "A型" then Some (first)
      else first_A rest

let person_1 = {name="太郎"; height=1.71; weight=64.0; birthday="12/16"; blood="O型"}
let person_2 = {name="Jiro"; height=1.61; weight=62.0; birthday="8/22"; blood="AB型"}
let person_3 = {name="Hanako"; height=1.53; weight=48.0; birthday="2/28"; blood="A型"}
let person_4 = {name="Junko"; height=1.48; weight=45.0; birthday="9/28"; blood="A型"}

let lst1 = []
let lst2 = [person_3]
let lst3 = [person_1]
let lst4 = [person_1; person_2; person_3; person_4]
let lst5 = [person_1; person_2]

let first_A_t1 = first_A lst1 = None
let first_A_t2 = first_A lst2 = Some (person_3)
let first_A_t3 = first_A lst3 = None
let first_A_t4 = first_A lst4 = Some (person_3)
let first_A_t5 = first_A lst5 = None
