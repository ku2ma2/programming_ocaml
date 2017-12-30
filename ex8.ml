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

