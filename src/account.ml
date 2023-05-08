open Unix
open Stocks

exception Broke
exception OverMemory
exception StockNotFound

type stock = {
  ticker : string;
  price : float;
}

type account = {
  stock_balance : float;
  cash_balance : float;
  portfolio : (stock * float) list;
  transaction_log : transaction list;
  watchlist : stock list;
}

and transaction = {
  time : string;
  type_of_transaction : string;
  share : float;
  stock : stock;
}

(** Converts the time into an easy-to-read month/day/year string format*)
let convert_unix_time (t : float) : string =
  let tm = localtime t in
  let day = string_of_int tm.tm_mday in
  let month = string_of_int (tm.tm_mon + 1) in
  let year = string_of_int (tm.tm_year + 1900) in
  day ^ "/" ^ month ^ "/" ^ year

let withdraw (amt : float) (acc : account) =
  let b1 = acc.stock_balance in
  let b2 = acc.cash_balance -. amt in
  if amt > acc.cash_balance then raise Broke
  else
    {
      stock_balance = b1;
      cash_balance = b2;
      portfolio = acc.portfolio;
      transaction_log = acc.transaction_log;
      watchlist = acc.watchlist;
    }

let deposit (amt : float) (acc : account) =
  let b1 = acc.stock_balance in
  let b2 = acc.cash_balance +. amt in
  {
    stock_balance = b1;
    cash_balance = b2;
    portfolio = acc.portfolio;
    transaction_log = acc.transaction_log;
    watchlist = acc.watchlist;
  }

let stock_to_string_pair stk = (stk.ticker, string_of_float stk.price)

let transaction_to_string_quint trans =
  let pp = stock_to_string_pair trans.stock in
  match pp with
  | a, b ->
      let tick = a in
      let pr = b in
      ( trans.time,
        trans.type_of_transaction,
        string_of_float trans.share,
        tick,
        pr )

let balance acc = string_of_float (acc.stock_balance +. acc.cash_balance)
let stock_balance acc = string_of_float acc.stock_balance

let only_stocks (acc : account) =
  List.map (fun ({ ticker; price }, _) -> ticker) acc.portfolio

let rec portfolio (port : (stock * float) list) =
  match port with
  | [] -> []
  | (h, q) :: t -> (h.ticker, h.price, q) :: portfolio t

let port_to_string port =
  let full_port = portfolio port in
  let rec to_string = function
    | [] -> ""
    | (ticker, price, q) :: t ->
        "(ticker = " ^ ticker ^ ", " ^ "average price = "
        ^ string_of_float price ^ ", " ^ "number of shares = "
        ^ string_of_float q ^ ") " ^ "\n" ^ to_string t
  in
  let list_text = to_string full_port in
  "{ " ^ list_text ^ "}"

let add_watchlist ticker acc =
  try
    let price2 = Stocks.get_ticker_price ticker in
    let rec check watchlist tic =
      match watchlist with
      | [] -> [ { ticker = tic; price = price2 } ]
      | { ticker = t1; price = p1 } :: t ->
          if t1 = tic then
            let new_stock = { ticker = t1; price = price2 } in
            new_stock :: t
          else { ticker = t1; price = p1 } :: check t tic
    in

    let new_watch = check acc.watchlist ticker in
    {
      stock_balance = acc.stock_balance;
      cash_balance = acc.cash_balance;
      portfolio = acc.portfolio;
      transaction_log = acc.transaction_log;
      watchlist = new_watch;
    }
  with exc -> raise (NoSuchStock ticker)

let remove_watchlist ticker acc =
  try
    let _ = Stocks.get_ticker_price ticker in
    let old_length = List.length acc.watchlist in
    let rec check watchlist tic =
      match watchlist with
      | [] -> []
      | { ticker = t1; price = p1 } :: t ->
          if t1 = tic then t else { ticker = t1; price = p1 } :: check t tic
    in

    let new_watch = check acc.watchlist ticker in
    if List.length new_watch = old_length then raise StockNotFound
    else
      {
        stock_balance = acc.stock_balance;
        cash_balance = acc.cash_balance;
        portfolio = acc.portfolio;
        transaction_log = acc.transaction_log;
        watchlist = new_watch;
      }
  with NoSuchStock ticker -> raise (NoSuchStock ticker)

let watch_to_string list =
  let rec to_string = function
    | [] -> ""
    | { ticker = t; price = p } :: t2 ->
        "(ticker = " ^ t ^ ", " ^ "average price = " ^ string_of_float p ^ ") "
        ^ "\n" ^ to_string t2
  in
  let list_text = to_string list in
  "{ " ^ list_text ^ "}"
