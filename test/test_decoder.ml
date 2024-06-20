open OUnit2
open Huffml.Huffman
open Huffml.Decoder

let create_test_tree () =
  Node(
    Node(Leaf('a', 3), Leaf('b', 2), 5),
    Leaf('c', 1),
    6
  )
(* Test decoding a simple encoded string *)
let test_decode_simple _ctx =
  let tree = create_test_tree () in
  let encoded = "00" in  (* Assuming "00" encodes 'a' based on the create_test_tree structure *)
  let decoded = decode encoded tree in
  assert_equal "a" decoded ~msg:"Failed to decode 'a' from '00'"

(* Test handling of an empty input string *)
let test_decode_empty _ctx =
  let tree = create_test_tree () in
  let decoded = decode "" tree in
  assert_equal "" decoded ~msg:"Decoding an empty string should return an empty result"

(* Test for incomplete or incorrect input *)
let test_decode_incomplete _ctx =
  let tree = create_test_tree () in
  let encoded = "0" in
  let decoded = decode encoded tree in
  assert_equal "" decoded ~msg:"Incomplete or incorrect input should handle gracefully"

let suite =
  "Huffman Decoding Tests" >::: [
    "decode simple" >:: test_decode_simple;
    "decode empty" >:: test_decode_empty;
    "decode incomplete" >:: test_decode_incomplete;
  ]