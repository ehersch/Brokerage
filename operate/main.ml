open Backend
open Stocks

let () =
  try
    let ticker_price = Lwt_main.run (get_ticker_price ticker) in
    print_endline "\nRemember, the API only has 5 calls per minute.\n";
    print_endline
      ("\nThe closing stock price for "
      ^ String.uppercase_ascii ticker
      ^ " yesterday was\n"
      ^ string_of_float ticker_price)
  with _ ->
    print_endline
      "Invalid Stock Ticker. Run make view_stock to begin again. \n\
      \      Remember, the API only has 5 calls per minute."
