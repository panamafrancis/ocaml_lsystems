open OUnit2
open Lsys

(*
 * test data
 *)

let empty_tree = Empty

let abc_tree = Operand{value="A"; next=Operand{
                value="B"; next=Operand{
                value="C"; next=Empty}}}

let ab_tree = Operand{value="A"; next=Operand{
                value="B"; next=Empty}}

let x_tree = Operand{value="X"; next=Empty}
 
let b_tree = Operand{value="B"; next=Empty}
let a_tree = Operand{value="A"; next=Empty}


(*ABA[BA[BA]]D*)
let branch_tree = 
                   Operand{value="A"; next=
                   Operand{value="B"; next=
                   Operand{value="A"; next=
                   Branch {left=
                           Operand{value="B"; next=
                           Operand{value="A"; next=
                               Branch {left=
                                       Operand{value="B"; next=
                                       Operand{value="A"; next=Empty}}; right=
                               Empty}}}; right= 
                   Operand{value="D"; next=Empty}}}}}


(***************************************************
 * L-Tree tests
 * *)

let test_equality _ =
    assert_equal abc_tree abc_tree

(* when inserting into a normal tree it should return the tree after 
 * the specified node
 *)
let test_insert_tree _ =
    let actual = insert_tree abc_tree x_tree "B" in
    let expected = Operand{ value="A"; next=Operand{
                            value="B"; next=Operand{
                            value="X"; next=Operand{
                            value="C"; next=Empty}}}} in
    assert_equal actual expected

(* when inserting into an empty tree it should just return the inserted tree *)
let test_insert_on_empty _ = 
    let actual = insert_tree empty_tree x_tree "fake node" in
    assert_equal actual x_tree

(* when inserting into a normal tree but after a non-existant node
 * it should just append the second tree to the first
 *)
let test_insert_at_bad_node _ = 
    let actual = insert_tree abc_tree x_tree "fake node" in
    let expected = Operand{ value="A"; next=Operand{
                            value="B"; next=Operand{
                            value="C"; next=Operand{
                            value="X"; next=Empty}}}} in
    assert_equal actual expected

let test_append_tree _ = 
    let actual = append_tree abc_tree x_tree in
    let expected = Operand{ value="A"; next=Operand{
                            value="B"; next=Operand{
                            value="C"; next=Operand{
                            value="X"; next=Empty}}}} in
    assert_equal actual expected

(* when appending onto an empty tree it should return the second*)
let test_append_tree_on_empty _ = 
    let actual = append_tree empty_tree x_tree in
    assert_equal actual x_tree

let test_ltree_to_string _ = 
    let actual = ltree_to_string branch_tree in
    let expected = "ABA[BA[BA]]D" in
    assert_equal actual expected


(***************************************************
 * L-System tests
 **)

let fib_lsys = {rules= [("A",ab_tree);("B",a_tree)];
                init= Operand{value="A"; next=Empty}};;

let test_lsys_equality _ = 
    assert_equal fib_lsys fib_lsys

let test_lsys_to_string _ = 
    let actual = lsys_to_string fib_lsys in
    let expected = "{ rules = [{axiom = A; rule = AB};\n{axiom = B; rule = A};\n]\n init = A\n}" in
    assert_equal actual expected

(***************************************************
 * L-System rewriting tests
 **)

let test_lsys_fib_rewriting_3 _ =
    let actual = ltree_to_string (rewrite fib_lsys 3).init in
    let expected = "ABAAB" in
    assert_equal actual expected

let test_lsys_fib_rewriting_10 _ =
    let actual = ltree_to_string (rewrite fib_lsys 10).init in
    let expected = "ABAABABAABAABABAABABAABAABABAABAABABAABABAABAABABAABABAABAABABAABAABABAABABAABAABABAABAABABAABABAABAABABAABABAABAABABAABAABABAABABAABAABABAABABA" in
    assert_equal actual expected

let suite = "LSystems tests" >::: 
    [
    "test_equality" >:: test_equality;
    "test_insert_tree" >:: test_insert_tree;
    "test_insert_on_empty" >:: test_insert_on_empty;
    "test_insert_at_bad_node" >:: test_insert_at_bad_node;
    "test_append_tree" >:: test_append_tree;
    "test_append_tree_on_empty" >:: test_append_tree_on_empty;
    "test_ltree_to_string" >:: test_ltree_to_string;
    "test_lsys_equality" >:: test_lsys_equality;
    "test_lsys_to_string" >:: test_lsys_to_string;
    "test_lsys_fib_rewriting_3" >:: test_lsys_fib_rewriting_3;
    "test_lsys_fib_rewriting_10" >:: test_lsys_fib_rewriting_10;
    ]

let _ =
    run_test_tt_main suite
