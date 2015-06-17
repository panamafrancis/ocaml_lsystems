open OUnit2
open Lsys

let abc_tree = Operand{value="A"; next=Operand{
                value="B"; next=Operand{
                value="C"; next=Empty}}}

let x_tree = Operand{value="X"; next=Empty}

let empty_tree = Empty

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

let suite = "LSystems tests" >::: 
    ["test_equality" >:: test_equality;
    "test_insert_tree" >:: test_insert_tree;
    "test_insert_on_empty" >:: test_insert_on_empty;
    "test_insert_at_bad_node" >:: test_insert_at_bad_node]

let _ =
    run_test_tt_main suite
