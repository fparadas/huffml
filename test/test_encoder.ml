open OUnit2
open Huffml.Huffman
open Huffml.Encoder

let create_huffman_tree () =
  Node (
    Node (Leaf ('a', 5), Leaf ('b', 2), 7),
    Leaf ('c', 10),
    17
  )

let test_encode_char_found _ctx =
  let tree = create_huffman_tree () in
  assert_equal "00" (encode_char 'a' tree) ~msg:"encode_char does not encode 'a' as expected";
  assert_equal "01" (encode_char 'b' tree) ~msg:"encode_char does not encode 'b' as expected";
  assert_equal "1" (encode_char 'c' tree) ~msg:"encode_char does not encode 'c' as expected"

let test_encode_char_not_found _ctx =
  let tree = create_huffman_tree () in
  assert_raises (Failure "Not found") (fun () -> encode_char 'd' tree)

let test_encode_string _ctx =
  let tree = create_huffman_tree () in
  let encoded = encode' "abc" tree in
  assert_equal "00011" encoded ~msg:"encode does not encode 'abc' as expected"

let test_full_encode _ctx = 
  let txt = "aabbc" in
  let expected_tree = Node (Leaf ('c', 1), Node (Leaf ('a', 2), Leaf ('b', 2), 4), 5) in
  let expected_encoding = "101011110" in
  let encoding, tree = encode txt in

  assert_equal expected_tree tree ~msg:"encode does not create a tree for 'aabbc' as expected";
  assert_equal expected_encoding encoding ~msg:"encode does not encode 'aabbc' as expected"

let suite =
  "Huffman Encoding Tests" >::: [
    "test_encode_char_found" >:: test_encode_char_found;
    "test_encode_char_not_found" >:: test_encode_char_not_found;
    "test_encode_string" >:: test_encode_string;
    "test_full_encode" >:: test_full_encode;
  ]