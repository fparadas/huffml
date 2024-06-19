open OUnit2
open Huffml.Huffman

let test_get_priority _ctx =
  let leaf = Leaf ('a', 5) in
  let node = Node (Leaf ('b', 3), Leaf ('c', 7), 10) in
  assert_equal 5 (get_priority leaf) ~msg:"Leaf priority should match";
  assert_equal 10 (get_priority node) ~msg:"Node priority should match"

let test_compare _ctx =
  let leaf1 = Leaf ('a', 5) in
  let leaf2 = Leaf ('b', 7) in
  let node1 = Node (Leaf ('c', 3), Leaf ('d', 7), 10) in
  let node2 = Node (Leaf ('e', 1), Leaf ('f', 2), 5) in
  assert_equal (-1) (compare leaf1 leaf2) ~msg:"leaf1 should be less than leaf2";
  assert_equal 1 (compare leaf2 leaf1) ~msg:"leaf2 should be greater than leaf1";
  assert_equal 1 (compare node1 node2) ~msg:"node1 should be greater than node2";
  assert_equal (-1) (compare node2 node1) ~msg:"node2 should be less than node1";
  assert_equal 0 (compare leaf1 node2) ~msg:"leaf1 and node2 should be equal in priority"


let leaf c p = Leaf (c, p)

(* Helper function to create a node *)
let node t1 t2 p = Node (t1, t2, p)

(* Test for insert_ordered *)
let test_insert_ordered _ctx =
  let trees = [leaf 'a' 1; leaf 'c' 3; leaf 'e' 5] in
  let new_tree = leaf 'b' 2 in
  let expected = [leaf 'a' 1; leaf 'b' 2; leaf 'c' 3; leaf 'e' 5] in
  assert_equal expected (insert_ordered new_tree trees)

(* Test for of_leaf_list *)
let test_of_leaf_list _ctx =
  let leaves = [leaf 'a' 1; leaf 'b' 1; leaf 'c' 2] in
  match of_leaf_list leaves with
  | Node (Node (Leaf ('a', 1), Leaf ('b', 1), 2), Leaf ('c', 2), 4) -> ()
  | _ -> assert_failure "of_leaf_list did not construct the expected tree structure"

(* Test for to_leaf_list *)
let test_to_leaf_list _ctx =
  let tree = Node (Node (Leaf ('a', 1), Leaf ('b', 1), 2), Leaf ('c', 2), 4) in
  let expected = [leaf 'a' 1; leaf 'b' 1; leaf 'c' 2] in
  assert_equal expected (to_leaf_list tree) ~printer:[%show: t list]


let test_init _test_ctxt =
  (* Test for empty string *)
  assert_raises (Failure "Empty list") (fun () -> init "");

  (* Test for single character string *)
  let tree_one_char = init "aaaa" in
  assert_equal (Leaf ('a', 4)) tree_one_char ~printer:[%show: t];

  (* Test for a more complex string *)
  let tree_complex = init "aabbc" in
  let expected = Node (Leaf ('c', 1), Node (Leaf ('a', 2), Leaf ('b', 2), 4), 5) in

  assert_equal expected tree_complex ~printer:[%show: t]
  
let suite =
  "TreeTests" >::: [
    "test_get_priority" >:: test_get_priority;
    "test_compare" >:: test_compare;
    "insert_ordered" >:: test_insert_ordered;
    "of_leaf_list" >:: test_of_leaf_list;
    "to_leaf_list" >:: test_to_leaf_list;
    "test_init" >:: test_init;
  ]