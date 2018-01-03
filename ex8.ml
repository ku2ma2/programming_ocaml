(* 問題 8.1: 本に関する情報を格納するレコード型 book_t を宣言せよ。 book_t 型にはフィールドとしてタイトル、著者名、出版社、値段、そしてISBNコードを持つようにせよ。この型を持つデータを3つ定義せよ。 *)
type book_t = {
    title: string;     (* タイトル *)
    author: string;    (* 著者名 *)
    publisher: string; (* 出版社 *)
    price: int;        (* 値段 *)
    isbn: string;      (* ISBN *)
}
let book1 = {
    title = "プログラミングの基礎";
    author = "浅井健一";
    publisher = "サイエンス社";
    price = 2484;
    isbn = "4781911609";
}
let book2 = {
    title = "アルゴリズムの基本";
    author = "トーマス・H・コルメン";
    publisher = "日経BP社";
    price = 2592;
    isbn = "482228543X";
}
let book3 = {
    title = "リーダブルコード ―より良いコードを書くためのシンプルで実践的なテクニック";
    author = "Dustin Boswell";
    publisher = "オライリージャパン";
    price = 2592;
    isbn = "4873115655";
}

(* 問題 8.1: お小遣い帳の情報を格納するレコード型 okozukai_t を宣言せよ。
okozukai_t 型にはフィールドとして買ったものの名前、値段、買った場所、
そして日付を持つようにせよ。この型を持つデータを3つ定義せよ。 *)
type okozukai_t = {
    item: string;
    price: int;
    shop: string;
    date: string;
}

let okozukai_1 = {
    item = "お菓子";
    price = 300;
    shop = "スーパー";
    date = "12/12";
}
let okozukai_2 = {
    item = "サランラップ";
    price = 190;
    shop = "薬局";
    date = "12/14";
}
let okozukai_3 = {
    item = "ゲーム";
    price = 2500;
    shop = "家電量販店";
    date = "12/22";
}

(* 問題 8.3: 各人の名前、身長(m)、体重(kg)、誕生日(月と日)と
血液型の情報を格納するレコード型 person_t を宣言せよ。
この型を持つデータを3つ定義せよ *)
type person_t = {
    name: string;
    height: float;
    weight: float;
    birthday:  string;
    blood: string;
}
let person_1 = {name="太郎"; height=1.71; weight=64.0; birthday="12/16"; blood="O型"}
let person_2 = {name="Jiro"; height=1.61; weight=62.0; birthday="8/22"; blood="AB型"}
let person_3 = {name="Hanako"; height=1.53; weight=48.0; birthday="2/28"; blood="A型"}
let person_4 = {name="Junko"; height=1.48; weight=45.0; birthday="9/28"; blood="A型"}

(* 問題 8.4: 問題8.3で定義した person_t 型のデータを受け取ったら
「◯◯さんの血液型は△型です」という形の文字列を返す関数
 ketsueki_hyoji をデザインレシピに従って作れ。 *)

(* 目的: person_t 型のデータを受け取ったら「◯◯さんの血液型は△型です」という形の文字列を返す関数 *)
(* ketsueki_hyouji : person_t -> string *)
let ketsueki_hyouji persion_t = match persion_t with 
    {name=n; height=h; weight=w; birthday=bd; blood=b} ->
        n ^ "さんの血液型は" ^ b ^ "です"

(* テスト *)
let ketsueki_hyouji_t1 = ketsueki_hyouji person_1 = "太郎さんの血液型はO型です"
let ketsueki_hyouji_t2 = ketsueki_hyouji person_2 = "Jiroさんの血液型はAB型です"
let ketsueki_hyouji_t3 = ketsueki_hyouji person_3 = "Hanakoさんの血液型はA型です"

