#use "eki12.ml";;

(* 問題 13.6: 直前に確定した駅 p（eki_t 型）と未確定の駅 q（eki_t 型）
を受け取ったら、p と q が直接つながっているかどうかを調べ、つながっていたら
q の最短距離の手前リストを必要に応じて更新したもの、つながっていなかったらもとの
q をそのまま返す関数 koushin1 をデザインレシピにしたがって作れ *)
(* koushin1 : eki_t -> eki_t list -> eki_t list *)
(* let koushin1 p q = match (p, q) with
    ({namae=pn; saitan_kyori=ps; temae_list=pt},{namae=qn; saitan_kyori=qs; temae_list=qt}) ->
        let kyori = get_ekikan_kyori pn qn global_ekikan_list in
            if kyori = infinity then q
            else if ps +. kyori < qs then {namae=qn; saitan_kyori=ps +. kyori; temae_list= qn :: pt}
            else q *)

(* 駅の例 *) 
let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} 
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} 
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} 
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} 
 
(* テスト *) 
(* let koushin1_t1 = koushin1 eki3 eki1 = eki1 
let koushin1_t2 = koushin1 eki3 eki2 = eki2 
let koushin1_t3 = koushin1 eki3 eki3 = eki3 
let koushin1_t4 = koushin1 eki3 eki4 = {namae="後楽園"; saitan_kyori = 1.8; temae_list = ["後楽園"; "茗荷谷"]} 
let koushin1_t5 = koushin1 eki2 eki1 = {namae="池袋"; saitan_kyori = 3.0; temae_list = ["池袋"; "新大塚"; "茗荷谷"]} 
let koushin1_t6 = koushin1 eki2 eki2 = eki2 
let koushin1_t7 = koushin1 eki2 eki3 = eki3
let koushin1_t8 = koushin1 eki2 eki4 = eki4 *)

(* 問題 13.7: 直前に確定した駅 p（eki_t型）と未確定の駅リスト v（eki_t list）を
受け取ったら、必要な更新処理を行ったあとの未確定の駅リストを返す関数
koushin をデザインレシピにしたがって作れ。 *)
(* 問題 14.13: koushin は koushin1 を局所的に定義している。
koushin1を局所定義する代わりに、名前の無い関数を使って koushin を定義し直せ *)
(* 問題 16.3: 問題 14.13 で作った koushin の引数をひとつ増やし、
駅間のリスト（ekikan_t list型）を受け取るようにせよ。
さらに koushin の中で get_ekikan_kyori を使う時にはこのリストを使うようにせよ。 *)

(* koushin : eki_t -> eki_t list -> eki_t list *)
let koushin p v ekikan_list = 
     List.map (
        fun q -> match (p, q) with
            ({namae=pn; saitan_kyori=ps; temae_list=pt},
            {namae=qn; saitan_kyori=qs; temae_list=qt}) ->
                let kyori = get_ekikan_kyori pn qn ekikan_list in
                    if kyori = infinity then q
                    else if ps +. kyori < qs then {namae=qn; saitan_kyori=ps +. kyori; temae_list= qn :: pt}
                    else q
     ) v

 
(* 駅リストの例 *) 
let koushin_lst = [eki1; eki2; eki3; eki4] 
 
(* テスト *) 
let koushin_t1 = koushin eki2 [] global_ekikan_list = [] 
let koushin_t2 = koushin eki2 koushin_lst global_ekikan_list = 
 [{namae="池袋"; saitan_kyori = 3.0; temae_list = ["池袋"; "新大塚"; "茗荷谷"]}; 
  eki2; eki3; eki4] 
 

