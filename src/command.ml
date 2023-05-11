type command =
  | Bal
  | Equity
  | Portfolio
  | Dep of float
  | Withdraw of float
  | Buy of (string * float)
  | Sell of (string * float)
  | View of string
  | ViewOption of string
  | Watchlist
  | WatchlistAdd of string
  | WatchlistRemove of string
  | Quit
  | History
  | Help
  | OptionsTickerHelp

exception Invalid
exception Empty

(** [parse str] parses a user's input into a [command]. The first word that is
    not an empty string becomes the command type listed in -help and the rest of
    the words if any become the phrase. Raises [Invalid] when input is not in
      the format of -command [number if required]. Example:
    - parse " -dep 500." is [Dep 500.0]]
    - parse "-bal" is [Bal] *)
let parse str =
  let split_str =
    List.filter
      (fun x -> String.length x > 0)
      (List.map String.trim (String.split_on_char ' ' str))
  in
  match split_str with
  | [ "-bal" ] -> Bal
  | [ "-equity" ] -> Equity
  | [ "-portfolio" ] -> Portfolio
  | "-dep" :: [ amt ] -> Dep (float_of_string amt)
  | "-withdraw" :: [ amt ] -> Withdraw (float_of_string amt)
  | "-buy" :: ticker :: [ num_shares ] ->
      Buy (String.uppercase_ascii ticker, float_of_string num_shares)
  | "-sell" :: ticker :: [ num_shares ] ->
      Sell (String.uppercase_ascii ticker, float_of_string num_shares)
  | [ "-help" ] -> Help
  | [ "-options_ticker_help" ] -> OptionsTickerHelp
  | [ "-quit" ] -> Quit
  | [ "-history" ] -> History
  | "-view" :: [ ticker ] -> View ticker
  | "-view_option" :: [ ticker ] -> ViewOption ticker
  | [ "-watchlist" ] -> Watchlist
  | "-watchlist" :: "add" :: [ ticker ] -> WatchlistAdd ticker
  | "-watchlist" :: "remove" :: [ ticker ] -> WatchlistRemove ticker
  | [ _ ] -> raise Invalid
  | _ :: tl -> raise Invalid
  | [] -> raise Empty
