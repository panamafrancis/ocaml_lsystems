open Core.Std

type ltree = Empty | 
             Operand of operand | 
             Branch of branch 
             and operand = { value : string; next: ltree }
             and branch = { left: ltree; right: ltree } 
(*
 * An L-System rule or an iteration is stored in a tree
 *   A
 *     B
 *      []
 *     A  B
 *      B  E
 *       E
 * Its string form would be AB[AB]B
 *)

type lsystem = {
            rules: (string*ltree) list; 
            init: ltree
}
(*
 * An L-System consists of a set of re-write rules and the initial state of the
 * system
 *
 *)

val rewrite : lsystem -> n:int -> lsystem 
(*
 *  Generate the state of the L-System after n iterations
 *)

val append_tree : ltree -> ltree -> ltree
(* 
 * return a new tree of [l1::l2]
 *)

val insert_tree : ltree -> ltree -> bytes -> ltree
(*
 * insert the second tree after the node with value x
 *)

val ltree_to_string : ltree -> string

val lsys_to_string : lsystem -> string



