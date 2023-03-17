open Stocks

let () =
  let ticker_price = Lwt_main.run (get_ticker_price ticker) in
  print_endline
    ("\nThe closing stock price for "
    ^ String.uppercase_ascii ticker
    ^ " yesterday was\n"
    ^ string_of_float ticker_price)
