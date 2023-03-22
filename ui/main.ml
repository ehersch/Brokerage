open Backend
open Stocks
open Command
open Account
let print_strings list_of_lists =
  List.iter (fun str_list ->
    List.iter (fun str ->
      print_endline str
    ) str_list
  ) list_of_lists
;;

let invalid_msg () =
  print_endline
  "Please enter in a valid prompt! Type -help to view all commands"
  (** terms lets the user decide on whether or not they agree to the terms and
      conditions in order to keep using our services*)
let rec prompt_command (curr_acc  : account) =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "Please enter a command. Type -help to view all valid commands\n";
  print_string "> ";
  match parse(read_line()) with

  | Portfolio -> 
    ANSITerminal.print_string [ ANSITerminal.red ]   
    "Here are all the stocks you own, their price, and your shares: \n";
    print_strings(portfolio curr_acc.portfolio);
    prompt_command(curr_acc)

  | Dep amt -> ANSITerminal.print_string [ ANSITerminal.red ]   
    ("Successfully deposited : " ^ string_of_float amt ^ "\n");
     let new_acc = deposit amt curr_acc in prompt_command(new_acc) 

  | Bal -> 
    ANSITerminal.print_string [ ANSITerminal.red ]   
    ("Your current balance (cash and stock worth combined) is: " 
    ^ (string_of_float curr_acc.stock_balance) ^ "\n");
    prompt_command(curr_acc) 
    
  | Help -> ANSITerminal.print_string [ ANSITerminal.red ]   
    "Available commands
    \n-bal
    \n-portfolio
    \n-dep [amt]
    \n-withdraw [amt]
    \n-view [ticker]
    \n-help
    \n-quit\n";
    prompt_command(curr_acc)

  | Quit -> ANSITerminal.print_string [ ANSITerminal.red ]   
    "Terminating brokerage simulation. Have a wonderful day! 
    If you want to run this program again, please type [make play]\n";

  | Withdraw amt -> 
    ANSITerminal.print_string [ ANSITerminal.red ]   
    ("Withdrawing $" ^ string_of_float amt ^ "from your account. Type -bal to see new balance");
      let new_acc = withdraw amt curr_acc in 
      prompt_command(new_acc)
      
  | View ticker ->
    try
      let _ = get_ticker_price ticker in prompt_command(curr_acc)
    with
    | NoSuchStock _ -> ANSITerminal.print_string [ ANSITerminal.red ]   
    "Your stock ticker does not exist. Try running another command again.";
    prompt_command(curr_acc)

  | Invalid -> 
    invalid_msg();
    prompt_command(curr_acc)

let aapl_stock = {ticker = "AAPL"; price = 135.0}
let meta_stock = {ticker = "META"; price = 175.0}
let fresh_acc_example_preloaded_stocks = {
  stock_balance = 0.;
  cash_balance = 0.;
  portfolio = [(aapl_stock, 2.0); (meta_stock, 3.0)];
}

(** new_user creates a new user within the UI and prints out an empty portfolio,
    giving the user the option to view stocks*)
    let new_user () =
      print_endline
        "You currently have no stocks owned with a balance of 0.0. Type -help to \
         view all our brokerage feature commands";
      ANSITerminal.print_string [ ANSITerminal.red ]
      "Type something to get started";
      print_string "> ";
      match read_line () with
      | h -> prompt_command(fresh_acc_example_preloaded_stocks)


let rec terms () =
  print_string
    "TERMS AND CONDITIONS AGREEMENT\n\
    \    \n\
    \    Welcome to Jame Street. By using our brokerage services, you agree to \
     be\n\
    \    bound by the following terms and conditions which govern your access \
     to and\n\
    \    use of our brokerage services, including any information, tools, \
     features, \n\
    \    or any other content offered through our platform.\n\
    \    \n\
    \    1. Eligibility\n\
    \    \n\
    \    You must be atleast 18 years old and have the legal capacity to enter \
     into\n\
    \    a binding agreement in order to use our services. By using our \
     services, you\n\
    \    represent and warrant that you meet these eligibility requirements\n\
    \    \n\
    \    2. Investment Advice\n\
    \    \n\
    \    We are not a registered investment adviser and do not provide \
     investment \n\
    \    advice. The information provided through our services is for \
     informational\n\
    \    purposes only and does not constitute investment advice or a \
     recommendation\n\
    \    to buy, sell, or hold any securities or other financial products\n\
    \    \n\
    \    3. Trading Risks\n\
    \    \n\
    \    You acknowledge and agree that trading securitires involves risks, \
     including\n\
    \    the risk of loss of principal. You are solely responsible for any \
     investment\n\
    \    decisions you make and for understanding the risks involved. \n\
    \    \n\
    \    4. Intellectual Property\n\
    \    \n\
    \    All content included in or made available through our services, \
     including\n\
    \    text, graphics, and software is the propety of Jame Street or its \
     licensors\n\
    \    and is protected by United States and international copyright laws\n\
    \    \n\
    \    5. Limitation of Liability\n\
    \    \n\
    \    You agree that we will not be liable for any direct, indirect, \
     incidental, \n\
    \    special, consequential, or exemplary damages arising out of or in \
     connection\n\
    \    with your use of our services, including any loss of profits, \
     goodwill, use,\n\
    \    or data.\n\
    \    \n\
    \    6. Termination\n\
    \    \n\
    \    We reserve the right to terminate your access to our services at any \
     time\n\
    \    and for any reason, without liability to you. \n\n\
    \    7. Modification of Terms and Conditions\n\n\
    \    We reserve the right to modify these terms and conditions at any time, \n\
    \    without notice to you. Your continued use of our services following \
     any such\n\
    \    modificaations constitutes your acceptance of the modified terms and\n\
    \    conditions. \n\n\
    \    By usng our services you acknowledge that you have read and \
     understand these\n\
    \    terms and conditions and agree to be bound by them.\n";
  ANSITerminal.print_string [ ANSITerminal.red ]
    "Do you agree to these terms and conditions? Please enter 'yes' or 'no' ";
  print_string "> ";
  match read_line () with
  | "yes" -> new_user ()
  | "no" ->
      print_string
        "You must agree to the terms and conditions to begin trading.\n"
  | _ ->
      print_string "Please enter a valid command.";
      print_string "> ";
      terms ()

(** age_check verifies that the user is atleast the legal age limit to engage in
    trading*)
let rec age_check ans =
  match ans with
  | "yes" -> terms ()
  | "no" ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Unfortunately, our terms abide by federal law where users must be \
         atleast\n\
         eighteen years old to access trading tools. Have a great day!.\n"
  | _ ->
      print_endline "Please enter a valid command.";
      print_string "> ";
      age_check (read_line ())


(** Starts the user interface for Jame Street*)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to Jame Street.\n";
  print_endline
    "We are the fastest growing trading firm in the industry using\n\
     innovative technology with fast user services.";
  print_endline "";

  ANSITerminal.print_string [ ANSITerminal.red ]
    "To begin trading, please indicate if you are atleast eighteen years old. \
     Please enter 'yes' or 'no' ";
  print_string "> ";
  match read_line () with
  | h -> (age_check h)

(* Execute the game engine. *)
let () = main ()