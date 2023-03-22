open Stocks
open OUnit2

(* Parse tests for valid user input commands*)
let parse_test (name : string) (input_str : string) (expected_output : command)
    : test =
  name >:: fun _ -> assert_equal expected_output (parse input_str)

(* Parse tests for user inputs that will raise exception Empty*)
let parse_excE_test (name : string) (input_str : string) : test =
  name >:: fun _ -> assert_raises Empty (fun () -> parse input_str)

(* Parse tests for user inputs that will raise Invalid for invalid commmands*)
let parse_excI_test (name : string) (input_str : string) : test =
  name >:: fun _ -> assert_raises Invalid (fun () -> input_str)

let parse_tests =
  [
    parse_test "Testing a valid single parameter command" "  -bal  " Bal;
    parse_test "Testing a valid command that uses multiple parameters"
      "-dep 500.0" (Dep [ "500.0" ]);
    parse_test "Testing a valid command with multiple spaces in between"
      " -withdraw     300.0 " (Withdraw [ "300.0" ]);
    parse_excE_test "Testing an empty string" "";
    parse_excE_test "Testing a string with only spaces" "      ";
    parse_excI_test "Testing a string with words after a no parameter command"
      "-bal invalid";
    parse_excI_test
      "Testing a string with no words after a parameter involved command" "-dep";
    parse_excI_test "Testing a string without a first word as a valid command"
      "let's -dep 500";
  ]

let suite = "test suite for A2" >::: List.flatten [ parse_tests ]
let _ = run_test_tt_main suite
