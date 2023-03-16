open Stocks

let () =
  let ticker_price = Lwt_main.run (get_ticker_price "AAPL") in
  print_endline
    ("\nStock price for AAPL yesterday is\n" ^ string_of_float ticker_price)
