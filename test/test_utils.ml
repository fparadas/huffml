open OUnit2
open Huffml.Utils
let test_count_repetitions_basic _ctx = 
  let txt = "ooocccddd" |> String.to_seq |> List.of_seq in
  assert_equal 3 (count_repetitions 'o' txt)

let test_count_repetitions_empty _ctx = 
  let txt = "" |> String.to_seq |> List.of_seq in
  assert_equal 0 (count_repetitions 'o' txt)


let count_repetitions = 
  "Count Repetitions Test" >::: [
    "Basic repetition counting" >:: test_count_repetitions_basic;
    "Empty String" >:: test_count_repetitions_empty
  ]

let test_frequency_basic _ctx =
  let txt = "ooocccddd" in
  let expected = List.sort compare [('o', 3); ('c', 3); ('d', 3)] in
  let result = List.sort compare (frequency txt) in
  assert_equal expected result ~printer:[%show: (char * int) list]

let test_frequency_empty _ctx =
  assert_equal [] (frequency "") ~printer:[%show: (char * int) list]

let test_frequency_single_char _ctx =
  let txt = String.make 20 'e' in
  assert_equal [('e', 20)] (frequency txt) ~printer:[%show: (char * int) list]

let test_frequency_mixed_chars _ctx =
  let txt = "Hello, World!" in
  let expected = List.sort compare [(' ', 1); (',', 1); ('!', 1); ('H', 1); ('W', 1); 
                  ('d', 1); ('e', 1); ('l', 3); ('o', 2); ('r', 1)] in
  let result = List.sort compare (frequency txt) in

  assert_equal expected result ~printer:[%show: (char * int) list]

let test_frequency_case_sensitivity _ctx =
  let txt = "Aa" in
  assert_equal [('A', 1); ('a', 1)] (frequency txt) ~printer:[%show: (char * int) list]

let frequency =
  "Frequency Tests" >::: [
    "Basic Frequency" >:: test_frequency_basic;
    "Empty String" >:: test_frequency_empty;
    "Single Character" >:: test_frequency_single_char;
    "Mixed Characters" >:: test_frequency_mixed_chars;
    "Case Sensitivity" >:: test_frequency_case_sensitivity;
  ]

let suite = 
  "Utils Tests" >::: [
    frequency;
    count_repetitions
  ]