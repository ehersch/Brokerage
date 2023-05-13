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
  watchlist : (stock * float) list;
  dep_with_log : dep_with list;
}

and transaction = {
  time : string;
  type_of_transaction : string;
  share : float;
  stock : stock;
}

and dep_with = {
  ctime : string;
  type_of : string;
  amount : float;
  prev_balance : float;
}

let create = []

let cash_to_string_quad cash =
  ( cash.ctime,
    cash.type_of,
    string_of_float cash.amount,
    string_of_float cash.prev_balance )

let dep_with_string log =
  if List.length log = 0 then "[]"
  else
    let rec to_string_helper my_log_1 =
      match my_log_1 with
      | [] -> ""
      | h :: t -> (
          match cash_to_string_quad h with
          | a, b, c, d ->
              "Time: " ^ a ^ "; Type: " ^ b ^ "; Amount: " ^ c
              ^ "; Previous balance: " ^ d ^ " \n " ^ to_string_helper t)
    in

    let cur_str = to_string_helper log in

    "[" ^ String.sub cur_str 0 (String.length cur_str - 3) ^ "]"

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
  let b2' = acc.cash_balance in
  if amt > acc.cash_balance then raise Broke
  else
    {
      stock_balance = b1;
      cash_balance = b2;
      portfolio = acc.portfolio;
      transaction_log = acc.transaction_log;
      watchlist = acc.watchlist;
      dep_with_log =
        (let dw1 =
           {
             ctime = convert_unix_time (time ());
             type_of = "Withdrawal";
             amount = amt;
             prev_balance = b1 +. b2';
           }
         in
         dw1 :: acc.dep_with_log);
    }

let deposit (amt : float) (acc : account) =
  let b1 = acc.stock_balance in
  let b2 = acc.cash_balance +. amt in
  let b2' = acc.cash_balance in
  {
    stock_balance = b1;
    cash_balance = b2;
    portfolio = acc.portfolio;
    transaction_log = acc.transaction_log;
    watchlist = acc.watchlist;
    dep_with_log =
      (let dw1 =
         {
           ctime = convert_unix_time (time ());
           type_of = "Deposit";
           amount = amt;
           prev_balance = b1 +. b2';
         }
       in
       dw1 :: acc.dep_with_log);
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

(** Calculates the current value of the portfolio [port]. *)
let rec find_stock_balance port =
  match port with
  | [] -> 0.
  | (stk, num) :: t ->
      (Stocks.get_ticker_price stk.ticker *. num) +. find_stock_balance t

let stock_balance acc = find_stock_balance acc.portfolio
let balance acc = acc.stock_balance +. acc.cash_balance
let cash_balance acc = acc.cash_balance

let only_stocks (acc : account) =
  List.map (fun ({ ticker; price }, _) -> ticker) acc.portfolio

let rec ret_portfolio (port : (stock * float) list) =
  match port with
  | [] -> []
  | (h, q) :: t -> (h.ticker, h.price, q) :: ret_portfolio t

let port_to_string port =
  let full_port = ret_portfolio port in
  let rec to_string = function
    | [] -> ""
    | (ticker, price, q) :: t ->
        "(ticker = " ^ ticker ^ ", " ^ "average purchase price = "
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
      | [] -> [ ({ ticker = tic; price = price2 }, price2) ]
      | ({ ticker = t1; price = p1 }, p2) :: t ->
          if t1 = tic then
            let new_stock = { ticker = t1; price = price2 } in
            (new_stock, price2) :: t
          else ({ ticker = t1; price = p1 }, p2) :: check t tic
    in

    let new_watch = check acc.watchlist ticker in
    {
      stock_balance = acc.stock_balance;
      cash_balance = acc.cash_balance;
      portfolio = acc.portfolio;
      transaction_log = acc.transaction_log;
      watchlist = new_watch;
      dep_with_log = acc.dep_with_log;
    }
  with exc -> raise (NoSuchStock ticker)

let remove_watchlist ticker acc =
  try
    let _ = Stocks.get_ticker_price ticker in
    let old_length = List.length acc.watchlist in
    let rec check watchlist tic =
      match watchlist with
      | [] -> []
      | ({ ticker = t1; price = p1 }, p2) :: t ->
          if t1 = tic then t
          else ({ ticker = t1; price = p1 }, p2) :: check t tic
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
        dep_with_log = acc.dep_with_log;
      }
  with NoSuchStock ticker -> raise (NoSuchStock ticker)

let watch_to_string list =
  let rec to_string = function
    | [] -> ""
    | ({ ticker = t; price = p }, p2) :: t2 ->
        let new_price = Stocks.get_ticker_price t in
        "(ticker = " ^ t ^ ", " ^ "price added at = " ^ string_of_float p ^ ", "
        ^ "current price = " ^ string_of_float new_price ^ ") " ^ "\n"
        ^ to_string t2
  in
  let list_text = to_string list in
  "{ " ^ list_text ^ "}"
