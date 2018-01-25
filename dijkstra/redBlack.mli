

type ('a, 'b) rb_tree_t

val empty : ('a, 'b) rb_tree_t
val balance : ('a, 'b) rb_tree_t -> ('a, 'b) rb_tree_t
val insert : ('a, 'b) rb_tree_t -> 'a -> 'b -> ('a, 'b) rb_tree_t
val search : ('a, 'b) rb_tree_t -> 'a -> 'b
