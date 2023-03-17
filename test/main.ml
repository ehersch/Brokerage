open Stocks
open OUnit2

let stock_info_tests = 

let suite =
  "test suite for Jame-Street"
  >::: List.flatten []

let _ = run_test_tt_main suite
