let () =
  let open OUnit2 in
  run_test_tt_main ("suite" >::: [
    Test_utils.suite;
    Test_huffman.suite;
    Test_encoder.suite;
  ])