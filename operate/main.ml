open Backend
open Stocks

let rec main num =
  try
    print_endline "\nEnter the ticker symbol of a stock: ";
    let ticker = String.uppercase_ascii (read_line ()) in
    let ticker_price = get_ticker_price ticker in
    print_endline "\nRemember, the API only has 5 calls per minute.";
    print_endline
      ("\nThe closing stock price for "
      ^ String.uppercase_ascii ticker
      ^ " yesterday was\n"
      ^ string_of_float ticker_price);
    main ()
  with
  | NoSuchStock _ ->
      print_endline "Invalid Stock Ticker. Please enter a valid ticker symbol.";
      main ()
  | _ ->
      print_endline
        "An error occurred while processing your request. Please try again.";
      main ()

let () = main ()
