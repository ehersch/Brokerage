open Brokerage
open Stocks
open Account
open Command
open OUnit2
open Log

let command_to_string (cmd : command) : string =
  match cmd with
  | Bal -> "Bal"
  | Cash -> "Cash"
  | Equity -> "Equity"
  | Portfolio -> "Portfolio"
  | Dep amt -> Printf.sprintf "Dep %f" amt
  | Withdraw amt -> Printf.sprintf "Withdraw %f" amt
  | Buy (ticker, num_shares) -> Printf.sprintf "Buy (%s, %f)" ticker num_shares
  | Sell (ticker, num_shares) ->
      Printf.sprintf "Sell (%s, %f)" ticker num_shares
  | View ticker -> Printf.sprintf "View %s" ticker
  | ViewOption ticker -> Printf.sprintf "ViewOption %s" ticker
  | Watchlist -> "Watchlist"
  | WatchlistAdd ticker -> Printf.sprintf "WatchlistAdd %s" ticker
  | WatchlistRemove ticker -> Printf.sprintf "WatchlistRemove %s" ticker
  | Cashflow -> "Cashflow"
  | Quit -> "Quit"
  | History -> "History"
  | Help -> "Help"
  | OptionsTickerHelp -> "OptionsTickerHelp"

(* Parse tests for valid user input commands*)
let parse_test (name : string) (input_str : string) (expected_output : command)
    : test =
  name >:: fun _ ->
  assert_equal ~printer:command_to_string expected_output (parse input_str)

(* Parse tests for user inputs that will raise exception Empty*)
let parse_excE_test (name : string) (input_str : string) : test =
  name >:: fun _ ->
  try
    let _ = parse input_str in
    assert_failure
      (Printf.sprintf
         "Expected Empty exception, but got successful parse for string: %s"
         input_str)
  with
  | Empty -> ()
  | e ->
      let msg = Printexc.to_string e in
      assert_failure
        (Printf.sprintf
           "Expected Empty exception, but got exception: %s for string: %s" msg
           input_str)

(* Parse tests for user inputs that will raise Invalid for invalid commands*)
let parse_excI_test (name : string) (input_str : string) : test =
  name >:: fun _ ->
  try
    let _ = parse input_str in
    assert_failure
      (Printf.sprintf
         "Expected Invalid exception, but got successful parse for string: %s"
         input_str)
  with
  | Invalid -> ()
  | e ->
      let msg = Printexc.to_string e in
      assert_failure
        (Printf.sprintf
           "Expected Invalid exception, but got exception: %s for string: %s"
           msg input_str)

let parse_tests =
  [
    parse_test "Testing a valid single parameter command" "  -bal  " Bal;
    parse_test "Testing a valid command that uses multiple parameters"
      "-dep\n       500.0" (Dep 500.0);
    parse_test "Testing a valid int command that uses multiple parameters"
      "-dep\n       500" (Dep 500.0);
    parse_test "Testing a valid command with\n       multiple spaces in between"
      " -withdraw 300.0 " (Withdraw 300.0);
    parse_excE_test "Testing an empty string" "";
    parse_excE_test "Testing a string with only spaces" "      ";
    parse_excI_test "Testing a string with words after a no parameter command"
      "-bal invalid";
    parse_excI_test
      "Testing a string with no words after a parameter involved command" "-dep";
    parse_excI_test "Testing a string without a first word as a valid command"
      "let's -dep 500";
    parse_test "Testing a valid Cash command" "  -cash  " Cash;
    parse_test "Testing a valid Equity command" "  -equity  " Equity;
    parse_test "Testing a valid Portfolio command" "  -portfolio  " Portfolio;
    parse_test "Testing a valid Buy command" "-buy AAPL 10.0"
      (Buy ("AAPL", 10.0));
    parse_test "Testing a valid Sell command" "-sell AAPL 5.0"
      (Sell ("AAPL", 5.0));
    parse_test "Testing a valid View command" "-view AAPL" (View "AAPL");
    parse_test "Testing a valid ViewOption command" "-view_option AAPL"
      (ViewOption "AAPL");
    parse_test "Testing a valid WatchlistAdd command" "-watchlist add AAPL"
      (WatchlistAdd "AAPL");
    parse_test "Testing a valid WatchlistRemove command"
      "-watchlist remove AAPL" (WatchlistRemove "AAPL");
    parse_test "Testing a valid Cashflow command" "  -cashflow  " Cashflow;
    parse_test "Testing a valid Quit command" "  -quit  " Quit;
    parse_test "Testing a valid History command" "  -history  " History;
    parse_test "Testing a valid Help command" "  -help  " Help;
    parse_test "Testing a valid OptionsTickerHelp command"
      "  -options_ticker_help  " OptionsTickerHelp;
    parse_excI_test "Testing a string with invalid command" "-invalid";
    parse_excI_test "Testing a string with invalid parameter for Dep command"
      "-dep invalid";
    parse_excI_test
      "Testing a string with invalid parameter for Withdraw command"
      "-withdraw invalid";
    parse_excI_test "Testing a string with invalid parameter for Buy command"
      "-buy AAPL invalid";
    parse_excI_test "Testing a string with invalid parameter for Sell command"
      "-sell AAPL invalid";
    parse_excI_test "Testing a string with missing parameter for View command"
      "-view";
    parse_excI_test
      "Testing a string with missing parameter for ViewOption command"
      "-view_option";
    parse_excI_test
      "Testing a string with missing parameter for WatchlistAdd command"
      "-watchlist add";
    parse_excI_test
      "Testing a string with missing parameter for WatchlistRemove command"
      "-watchlist remove";
    parse_excI_test "Testing a command with too many parameters"
      "-dep 500.0 extra";
    parse_excI_test "Testing a command with invalid case" "-Dep 500.0";
    parse_test "Testing a command with many decimal places"
      "-dep 500.0000000001" (Dep 500.0000000001);
    parse_test "Testing a command with negative number" "-dep -500.0"
      (Dep (-500.0));
    parse_test "Testing a command with negative number for withdraw"
      "-withdraw -300.0" (Withdraw (-300.0));
    parse_test "Testing a command with negative number for buy"
      "-buy AAPL -10.0"
      (Buy ("AAPL", -10.0));
    parse_test "Testing a command with negative number for sell"
      "-sell AAPL -5.0"
      (Sell ("AAPL", -5.0));
    parse_test "Testing a valid Buy command without decimal point"
      "-buy AAPL 10"
      (Buy ("AAPL", 10.0));
    parse_test "Testing a valid Sell command without decimal point"
      "-sell AAPL 5"
      (Sell ("AAPL", 5.0));
    parse_test "Testing a valid View command with lower case ticker"
      "-view aapl" (View "aapl");
    parse_test "Testing a valid ViewOption command with lower case ticker"
      "-view_option aapl" (ViewOption "aapl");
    parse_test "Testing a valid WatchlistAdd command with lower case ticker"
      "-watchlist add aapl" (WatchlistAdd "aapl");
    parse_test "Testing a valid WatchlistRemove command with lower case ticker"
      "-watchlist remove aapl" (WatchlistRemove "aapl");
  ]

let stock1 = { ticker = "GOOG"; price = 1000.0 }
let stock2 = { ticker = "AAPL"; price = 150.0 }
let stock3 = { ticker = "MSFT"; price = 200.0 }

let transaction1 =
  {
    time = "12/31/2023";
    type_of_transaction = "Buy";
    share = 5.0;
    stock = stock1;
  }

let transaction2 =
  {
    time = "01/01/2024";
    type_of_transaction = "Sell";
    share = 2.0;
    stock = stock2;
  }

let transaction3 =
  {
    time = "01/01/2025";
    type_of_transaction = "Buy";
    share = 5.0;
    stock = stock3;
  }

let dep_with1 =
  {
    ctime = "01/01/2025";
    type_of = "Deposit";
    amount = 5000.0;
    prev_balance = 10000.0;
  }

let account_test2 =
  {
    stock_balance = 0.0;
    cash_balance = 10000.0;
    portfolio = [ (stock1, 5.0); (stock2, 3.0) ];
    transaction_log = [ transaction1; transaction2; transaction3 ];
    watchlist = [];
    dep_with_log = [ dep_with1 ];
  }

let account_tests =
  [
    ( "test withdrawal" >:: fun _ ->
      let acc = withdraw 500.0 account_test2 in
      assert_equal ~printer:string_of_float 9500.0 acc.cash_balance );
    ( "test deposit" >:: fun _ ->
      let acc = deposit 500.0 account_test2 in
      assert_equal ~printer:string_of_float 10500.0 acc.cash_balance );
    ( "test stock balance calculation" >:: fun _ ->
      let actual_balance =
        List.fold_left
          (fun acc (stock, quantity) -> acc +. (stock.price *. quantity))
          0.0 account_test2.portfolio
      in
      assert_equal ~printer:string_of_float 5450.0 actual_balance );
    ( "test balance" >:: fun _ ->
      assert_equal ~printer:string_of_float 10000.0 (balance account_test2) );
    ( "test cash balance" >:: fun _ ->
      assert_equal ~printer:string_of_float 10000.0 (cash_balance account_test2)
    );
    ( "test only stocks" >:: fun _ ->
      assert_equal
        ~printer:(fun l -> String.concat ", " l)
        [ "GOOG"; "AAPL" ]
        (only_stocks account_test2) );
    ( "test ret portfolio" >:: fun _ ->
      assert_equal
        ~printer:(fun l ->
          String.concat ", "
            (List.map (fun (s, p, q) -> s ^ ": " ^ string_of_float q) l))
        [ ("GOOG", 1000.0, 5.0); ("AAPL", 150.0, 3.0) ]
        (ret_portfolio account_test2.portfolio) );
  ]

(* More account tests *)
let deposit_test (name : string) (amt : float) (acc : account)
    (expected_output : account) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_float expected_output.cash_balance
    (deposit amt acc).cash_balance

let withdraw_test (name : string) (amt : float) (acc : account)
    (expected_output : account) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_float expected_output.cash_balance
    (withdraw amt acc).cash_balance

let withdraw_fail_test (name : string) (amt : float) (acc : account) : test =
  name >:: fun _ -> assert_raises Broke (fun () -> withdraw amt acc)

let add_watchlist_test (name : string) (ticker : string) (acc : account)
    (expected_output : account) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int
    (List.length expected_output.watchlist)
    (List.length (add_watchlist ticker acc).watchlist)

let remove_watchlist_test (name : string) (ticker : string) (acc : account)
    (expected_output : account) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_int
    (List.length expected_output.watchlist)
    (List.length (remove_watchlist ticker acc).watchlist)

let remove_watchlist_fail_test (name : string) (ticker : string) (acc : account)
    : test =
  name >:: fun _ ->
  assert_raises StockNotFound (fun () -> remove_watchlist ticker acc)

let create_account =
  {
    stock_balance = 0.;
    cash_balance = 0.;
    portfolio = [];
    transaction_log = [];
    watchlist = [];
    dep_with_log = [];
  }

let account_tests_live_data =
  [
    deposit_test "Deposit 500 to an empty account" 500.0 create_account
      {
        create_account with
        cash_balance = 500.0;
        dep_with_log =
          [
            {
              ctime = "1621027200";
              type_of = "Deposit";
              amount = 500.0;
              prev_balance = 0.0;
            };
          ];
      };
    withdraw_test "Withdraw 250 from an account with 500" 250.0
      {
        create_account with
        cash_balance = 500.0;
        dep_with_log =
          [
            {
              ctime = "1621027200";
              type_of = "Deposit";
              amount = 500.0;
              prev_balance = 0.0;
            };
          ];
      }
      {
        create_account with
        cash_balance = 250.0;
        dep_with_log =
          [
            {
              ctime = "1621027200";
              type_of = "Withdrawal";
              amount = 250.0;
              prev_balance = 500.0;
            };
            {
              ctime = "1621027200";
              type_of = "Deposit";
              amount = 500.0;
              prev_balance = 0.0;
            };
          ];
      };
    withdraw_fail_test "Attempt to withdraw 750 from an account with 500" 750.0
      {
        create_account with
        cash_balance = 500.0;
        dep_with_log =
          [
            {
              ctime = "1621027200";
              type_of = "Deposit";
              amount = 500.0;
              prev_balance = 0.0;
            };
          ];
      };
    add_watchlist_test "Add AAPL to watchlist" "AAPL" create_account
      {
        create_account with
        watchlist = [ ({ ticker = "AAPL"; price = 10.0 }, 10.0) ];
      };
    remove_watchlist_test "Remove AAPL from watchlist" "AAPL"
      {
        create_account with
        watchlist = [ ({ ticker = "AAPL"; price = 10.0 }, 10.0) ];
      }
      create_account;
    remove_watchlist_fail_test
      "Attempt to remove non-existing ticker from watchlist" "AAPL"
      create_account;
  ]

let suite =
  "test suite for Jame Street" >::: List.flatten [ parse_tests; account_tests ]

let _ = run_test_tt_main suite
